#!/usr/bin/env python3
"""
build_proofnet_jsonl.py

Construct a JSONL dataset from formal Lean files and informal TeX files.
Each entry contains:
  - name: combined source filename and theorem/instance name
  - source: Lean file name
  - header: Lean file header before first theorem/instance
  - formal_statement: theorem content with trailing 'sorry'
  - formal_proof: proof text or None if contains 'sorry'
  - informal_stmt: text after \\paragraph{...}
  - informal_proof: text inside \\begin{proof}...\\end{proof}

Missing informal entries are logged.
"""
import os
import re
import json
from typing import List, Dict, Set, Tuple


def extract_local_bindings(declaration_text: str) -> Set[str]:
    """Extract local variable names from a declaration."""
    local_vars = set()
    
    # Extract type parameters: {X : Type*}, [TopologicalSpace X]
    type_params = re.findall(r'[{\[]([^:\]}]+)\s*:', declaration_text)
    for param in type_params:
        local_vars.update(name.strip() for name in param.split())
    
    # Extract regular parameters: (f : ℕ → ℕ) (p : ℕ → ℝ)
    param_matches = re.findall(r'\(([^:)]+)\s*:', declaration_text)
    for param in param_matches:
        local_vars.update(name.strip() for name in param.split())
    
    # Extract existential/universal quantifiers: ∃ (X I : Type*), ∀ i, etc.
    quant_matches = re.findall(r'[∃∀]\s*\(?([^:,)]+)(?:\s*:\s*[^,)]+)?[,)]', declaration_text)
    for quant in quant_matches:
        local_vars.update(name.strip() for name in quant.split())
    
    # Extract lambda bindings: λ n =>, λ (k : ℕ) =>
    lambda_matches = re.findall(r'λ\s*\(?([^:)=]+)(?:\s*:\s*[^)=]+)?\)??\s*=>', declaration_text)
    for lam in lambda_matches:
        local_vars.update(name.strip() for name in lam.split())
    
    return local_vars


def find_dependencies(declaration_text: str, local_vars: Set[str], available_defs: Dict[str, int]) -> List[str]:
    """Find dependencies of a declaration, excluding local variables."""
    dependencies = []
    
    # Extract all identifiers (word boundaries)
    identifiers = re.findall(r'\b[a-zA-Z_][a-zA-Z0-9_]*\b', declaration_text)
    
    seen = set()
    for identifier in identifiers:
        if (identifier not in local_vars and 
            identifier not in seen and
            identifier in available_defs):
            dependencies.append(identifier)
            seen.add(identifier)
    
    # Sort by position in file (available_defs maps name to line number)
    dependencies.sort(key=lambda x: available_defs[x])
    return dependencies


def parse_declarations(lines: List[str]) -> List[Dict]:
    """Parse all declarations (def, theorem, instance, noncomputable def) from Lean file."""
    declarations = []
    i = 0
    n = len(lines)
    
    while i < n:
        line = lines[i]
        # Match def, theorem, instance, or noncomputable def
        match = re.match(r'^\s*(?:noncomputable\s+)?(def|theorem|instance)\s+(\w+)', line)
        if match:
            decl_type = match.group(1)
            name = match.group(2)
            start_line = i
            
            # Collect the complete declaration until blank line
            block = []
            while i < n and lines[i].strip() != '':
                block.append(lines[i])
                i += 1
            
            full_text = '\n'.join(block)
            
            declarations.append({
                'type': decl_type,
                'name': name,
                'text': full_text,
                'start_line': start_line
            })
        else:
            i += 1
    
    return declarations


def build_dependency_chain(target_decl: Dict, all_declarations: List[Dict]) -> List[Dict]:
    """Build the complete dependency chain for a target declaration."""
    # Build map of available definitions (only defs that appear before target)
    available_defs = {}
    for decl in all_declarations:
        if decl['start_line'] < target_decl['start_line'] and decl['type'] == 'def':
            available_defs[decl['name']] = decl['start_line']
    
    # Extract local bindings from target declaration
    local_vars = extract_local_bindings(target_decl['text'])
    
    # Find direct dependencies
    direct_deps = find_dependencies(target_decl['text'], local_vars, available_defs)
    
    # Recursively collect dependencies of dependencies
    all_deps = []
    processed = set()
    
    def collect_deps(dep_name: str):
        if dep_name in processed:
            return
        processed.add(dep_name)
        
        # Find the declaration for this dependency
        dep_decl = None
        for decl in all_declarations:
            if decl['name'] == dep_name and decl['type'] == 'def':
                dep_decl = decl
                break
        
        if dep_decl:
            # Get local vars for this dependency
            dep_local_vars = extract_local_bindings(dep_decl['text'])
            # Find its dependencies
            sub_deps = find_dependencies(dep_decl['text'], dep_local_vars, available_defs)
            
            # Recursively collect sub-dependencies first
            for sub_dep in sub_deps:
                collect_deps(sub_dep)
            
            # Add this dependency
            all_deps.append(dep_decl)
    
    # Collect all dependencies
    for dep in direct_deps:
        collect_deps(dep)
    
    return all_deps


def parse_formal_file(file_path):
    """Parse a Lean file into header and a list of theorem blocks with dependencies."""
    lines = open(file_path, 'r', encoding='utf-8').read().splitlines()
    
    # Parse all declarations
    all_declarations = parse_declarations(lines)
    
    # Extract header before first theorem, instance, def, or noncomputable def
    idx = 0
    while idx < len(lines) and not re.match(r"^\s*(?:noncomputable\s+)?(def|theorem|instance)\s+", lines[idx]):
        idx += 1
    header = "\n".join(lines[:idx])

    # File prefix for name
    base = os.path.basename(file_path)
    file_prefix = os.path.splitext(base)[0].replace('-', '_')

    entries = []
    
    # Process only theorems and instances
    for decl in all_declarations:
        if decl['type'] in ['theorem', 'instance']:
            orig_name = decl['name']
            full_text = decl['text']
            
            # Build dependency chain
            dep_chain = build_dependency_chain(decl, all_declarations)
            
            # Split theorem at first ':='
            if ':=' in full_text:
                head, sep, tail = full_text.partition(':=')
                theorem_statement = f"{head}{sep} sorry"
                proof = tail.strip() or None
            else:
                # Handle case where theorem has no proof (only declaration)
                theorem_statement = full_text
                if not theorem_statement.endswith('sorry'):
                    theorem_statement += ' := sorry'
                proof = None
            
            # Build complete formal statement with dependencies
            formal_parts = []
            
            # Add dependencies first
            for dep in dep_chain:
                formal_parts.append(dep['text'])
            
            # Add the theorem itself
            formal_parts.append(theorem_statement)
            
            # Join with double newlines for readability
            complete_formal_statement = '\n\n'.join(formal_parts)
            
            # New name
            new_name = f"{file_prefix}_{orig_name}"
            
            # Update names in the complete statement
            complete_formal_statement = complete_formal_statement.replace(orig_name, new_name)
            if proof:
                proof = proof.replace(orig_name, new_name)
            
            entries.append({
                'orig_name': orig_name,
                'name': new_name,
                'formal_statement': complete_formal_statement,
                'formal_proof': None if (proof and 'sorry' in proof) else proof
            })
    
    return header, os.path.basename(file_path), entries


def parse_informal_tex(tex_text, orig_name):
    """Extract informal statement and proof from TeX text for given original name."""
    # build title: capitalize first part, join numbers with dots
    parts = orig_name.split('_')
    title = parts[0].capitalize() + ' ' + '.'.join(parts[1:])
    # extract paragraph
    pat_stmt = re.compile(rf"\\paragraph\{{\s*{re.escape(title)}\s*\}}(.*?)(?=(\\paragraph|$))", re.DOTALL)
    m_stmt = pat_stmt.search(tex_text)
    informal_stmt = m_stmt.group(1).strip() if m_stmt else None
    # extract proof environment
    pat_proof = re.compile(r"\\begin\{proof\}(.*?)\\end\{proof\}", re.DOTALL)
    m_proof = pat_proof.search(tex_text)
    informal_proof = m_proof.group(1).strip().replace('\n', ' ').strip() if m_proof else None
    return informal_stmt, informal_proof


def main():
    base = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    formal_dir = os.path.join(base, 'formal')
    informal_dir = os.path.join(base, 'informal')
    output_file = os.path.join(base, 'proofnet_lean4.jsonl')
    log_missing = []

    # load all informal texts
    informal_texts = {}
    for fname in os.listdir(informal_dir):
        if fname.endswith('.tex'):
            key = os.path.splitext(fname)[0]
            path = os.path.join(informal_dir, fname)
            informal_texts[key] = open(path, 'r', encoding='utf-8').read()

    # build entries and write jsonl
    with open(output_file, 'w', encoding='utf-8') as outp:
        for f_name in os.listdir(formal_dir):
            if not f_name.endswith('.lean'): continue
            fpath = os.path.join(formal_dir, f_name)
            # determine the key for informal tex lookup by file name without extension
            file_key = os.path.splitext(f_name)[0]
            header, source, entries = parse_formal_file(fpath)
            for e in entries:
                orig = e['orig_name']
                # lookup informal tex by the formal file base name
                tex_text = informal_texts.get(file_key)
                if tex_text:
                    inf_stmt, inf_proof = parse_informal_tex(tex_text, orig)
                else:
                    inf_stmt = inf_proof = None
                    log_missing.append(e['name'])
                record = {
                    'name': e['name'],
                    'source': source,
                    'header': header,
                    'formal_statement': e['formal_statement'],
                    'formal_proof': e['formal_proof'],
                    'informal_stmt': inf_stmt,
                    'informal_proof': inf_proof
                }
                json.dump(record, outp, ensure_ascii=False)
                outp.write('\n')

    # log missing informal
    log_file = os.path.join(base, 'script', 'missing_informal.log')
    with open(log_file, 'w', encoding='utf-8') as lf:
        for nm in sorted(set(log_missing)):
            lf.write(f"{nm}\n")
    print(f"Generated {output_file}, logged {len(set(log_missing))} missing informal entries to {log_file}")

if __name__ == '__main__':
    main()

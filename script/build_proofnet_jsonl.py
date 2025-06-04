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


def parse_formal_file(file_path):
    """Parse a Lean file into header and a list of theorem blocks."""
    lines = open(file_path, 'r', encoding='utf-8').read().splitlines()
    # extract header before first theorem or instance
    idx = 0
    while idx < len(lines) and not re.match(r"^\s*(theorem|instance) ", lines[idx]):
        idx += 1
    header = "\n".join(lines[:idx])

    entries = []
    n = len(lines)
    i = idx
    # file prefix for name
    base = os.path.basename(file_path)
    file_prefix = os.path.splitext(base)[0].replace('-', '_')

    while i < n:
        line = lines[i]
        m_name = re.match(r"\s*(theorem|instance)\s+(\w+)", line)
        if m_name:
            orig_name = m_name.group(2)
            # collect block until blank line
            block = []
            while i < n and lines[i].strip() != '':
                block.append(lines[i])
                i += 1
            full_text = "\n".join(block)
            # split at first ':='
            if ':=' in full_text:
                head, sep, tail = full_text.partition(':=')
                content = f"{head}{sep} sorry"
                proof = tail.strip() or None
            else:
                raise ValueError(f"Invalid theorem format in {file_path} at line {i + 1}")
            # new name
            new_name = f"{file_prefix}_{orig_name}"
            # update content and proof names
            content = content.replace(orig_name, new_name)
            if proof:
                proof = proof.replace(orig_name, new_name)
            entries.append({
                'orig_name': orig_name,
                'name': new_name,
                'formal_statement': content,
                'formal_proof': None if (proof and 'sorry' in proof) else proof
            })
        else:
            i += 1
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

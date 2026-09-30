import re, json, csv, html

SRC = "/Users/lorenzoruffino/Downloads/Datafile-subset/Datafile-subset codebook.html"
OUT_JSON = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS/input/codebook_variabili.json"
OUT_CSV = "/Users/lorenzoruffino/Documents/Progetti/data-viz/Opinioni economiche ESS/input/codebook_variabili.csv"

data = open(SRC, encoding="utf-8").read()

# Split on variable detail sections: <h3 id="varname">varname </h3>
blocks = re.split(r'<h3 id="([A-Za-z0-9_]+)">', data)
# blocks[0] = preamble; then alternating (varname, content)
variables = {}
for i in range(1, len(blocks), 2):
    name = blocks[i]
    content = blocks[i+1]
    # label: first <div>...</div> after the h3 close
    m = re.search(r'</h3>\s*<div>(.*?)</div>', content, re.S)
    label = html.unescape(m.group(1)).strip() if m else ""
    # question text: variable-meta-string divs
    metas = re.findall(r'<div class="variable-meta-string">(.*?)</div>', content, re.S)
    question = " | ".join(html.unescape(x).strip() for x in metas)
    # value rows
    rows = re.findall(r'<tr>\s*<td class="nowrap">([^<]*)</td>\s*<td>(.*?)</td>\s*</tr>', content, re.S)
    values = []
    for val, cat in rows:
        cat = html.unescape(re.sub(r'<[^>]+>', '', cat)).strip()
        missing = cat.endswith('*')
        values.append({"value": val.strip(), "category": cat.rstrip('*').strip(), "missing": missing})
    if name in variables:
        continue
    variables[name] = {"label": label, "question": question, "values": values}

with open(OUT_JSON, "w", encoding="utf-8") as f:
    json.dump(variables, f, ensure_ascii=False, indent=1)

with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["variable", "label", "valid_values", "missing_values"])
    for name, v in variables.items():
        valid = ";".join(x["value"] for x in v["values"] if not x["missing"])
        miss = ";".join(x["value"] for x in v["values"] if x["missing"])
        w.writerow([name, v["label"], valid, miss])

print(f"Parsed {len(variables)} variables")
for k in ["gincdif","ginveco","basinc","topinfr","gvslvue","needtru","tporgwk","hinctnta","mnactic"]:
    v = variables.get(k)
    print(k, "->", v["label"] if v else "MISSING", "| valid:", [x["value"] for x in v["values"] if not x["missing"]][:12] if v else "")

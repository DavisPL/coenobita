import json, copy

f = open("protected-functions.txt", "r")
raw = f.read()

# Separate into rows
rows = raw.split("\n")
data = {}

for row in rows:
    (name, args) = row.split("\t")
    args = int(args)
    
    arg_ty = {
        "property": {
            "authors": "Universal",
            "suppliers": {
                "Specific": [
                    "root"
                ]
            }
        },
        "kind": "Infer"
    }

    fn_data = {
        "property": {
            "authors": "Universal",
            "suppliers": "Universal"
        },
        "kind": {
            "Fn": []
        }
    }

    if args > 0:
        fn_data["kind"]["Fn"].append(None)
    else:
        fn_data["kind"]["Fn"].append({
            "property": {
                "authors": "Universal",
                "suppliers": {
                    "Specific": [
                        "root"
                    ]
                }
            },
            "kind": "Infer"
        })

    arg_list = []
    for _ in range(args):
        arg_list.append(copy.deepcopy(arg_ty))
    
    fn_data["kind"]["Fn"].append(arg_list)

    fn_data["kind"]["Fn"].append({
        "property": {
            "authors": "Universal",
            "suppliers": "Universal"
        },
        "kind": "Infer"
    })

    data[name] = fn_data

f = open("annotations/provenance.json", "w")
json.dump(data, f)
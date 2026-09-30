import json,itertools,sys
def load(fn):
    d=json.load(open(fn))
    dims=d['id']; sz=d['size']
    labs=[list(d['dimension'][k]['category']['index'].keys()) for k in dims]
    # index order sorted by index value
    labs=[sorted(d['dimension'][k]['category']['index'],key=lambda c:d['dimension'][k]['category']['index'][c]) for k in dims]
    out={}
    for key,val in d['value'].items():
        i=int(key); coords=[]
        for s in reversed(sz):
            coords.append(i%s); i//=s
        coords=coords[::-1]
        out[tuple(labs[j][c] for j,c in enumerate(coords))]=val
    return dims,labs,out

import json, urllib.request, itertools
def get(ds, q):
    url=f"https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/{ds}?{q}&lang=en"
    d=json.load(urllib.request.urlopen(url))
    ids,size=d['id'],d['size']; idx={k:{v:kk for kk,v in d['dimension'][k]['category']['index'].items()} for k in ids}
    out={}
    for key,val in d['value'].items():
        k=int(key); c=[]
        for s in reversed(size): c.append(k%s); k//=s
        c=c[::-1]; out[tuple(idx[ids[j]][c[j]] for j in range(len(ids)))]=val
    return ids,out
items=["D1PAY","P2","D62PAY","D632PAY","D41PAY","P51G","D3PAY","D7PAY","D9PAY","TE","OTE"]
q="geo=IT&unit=MIO_EUR&sector=S13&time=2024&"+"&".join("na_item="+i for i in items)+"&"+"&".join(f"cofog99=GF{n:02d}" for n in range(1,11))+"&cofog99=TOTAL"
ids,ex=get("gov_10a_exp",q)
q2="geo=IT&unit=MIO_EUR&sector=S13&time=2024&time=2025&"+"&".join("na_item="+i for i in items)
ids2,ma=get("gov_10a_main",q2)
def m(item,year): return ma.get(('A','MIO_EUR','S13',item,'IT',str(year)))
print("aggregati:", {i:(m(i,2024),m(i,2025)) for i in items})
g={i:m(i,2025)/m(i,2024) for i in items if m(i,2024)}
names={1:"Servizi generali",2:"Difesa",3:"Ordine pubblico",4:"Affari economici",5:"Ambiente",6:"Abitazioni",7:"Sanità",8:"Ricreazione e cultura",9:"Istruzione",10:"Protezione sociale"}
comp=["D1PAY","P2","D62PAY","D632PAY","D41PAY","P51G","D3PAY","D7PAY","D9PAY"]
def e(f,i): return ex.get(('A','MIO_EUR','S13',f,i,'IT','2024'),0) or 0
tot24=e("TOTAL","TE"); res={}
for n in range(1,11):
    f=f"GF{n:02d}"; te=e(f,"TE"); parts=sum(e(f,i) for i in comp); other=te-parts
    est=sum(e(f,i)*g[i] for i in comp)+other*g["TE"]
    res[names[n]]=(te,est)
sum25=sum(v[1] for v in res.values())
scale=m("TE",2025)/sum25
print("tot 2024 cofog", tot24, "tot 2025 main", m("TE",2025), "somma stime", round(sum25), "riscalatura", round(scale,4))
for k,(a,b) in res.items():
    b*=scale; print(f"{k:22s} 2024 {a/1000:7.1f} ({a/tot24*100:4.1f}%)  stima2025 {b/1000:7.1f} ({b/m('TE',2025)*100:4.1f}%)  var {((b/a)-1)*100:+5.1f}%")
d24=e("GF02","TE"); print("difesa 2024 % Pil:", round(d24/2210595*100,3))
print("interessi 2025:", m("D41PAY",2025))

# --- Voci del treemap 2025 -------------------------------------------------
q3="geo=IT&unit=MIO_EUR&sector=S13&time=2024&"+"&".join("na_item="+i for i in items)+"&cofog99=GF1002&cofog99=GF0107"
_,ex2=get("gov_10a_exp",q3); ex.update(ex2)
def stima(f):
    te=e(f,"TE"); parts=sum(e(f,i) for i in comp)
    return sum(e(f,i)*g[i] for i in comp)+(te-parts)*g["TE"]
v={}
for n in range(1,11): v[f"GF{n:02d}"]=stima(f"GF{n:02d}")
v["GF1002"]=stima("GF1002"); v["GF0107"]=stima("GF0107")
v["GF02_P51G"]=e("GF02","P51G")*g["P51G"]
sc=m("TE",2025)/sum(v[f"GF{n:02d}"] for n in range(1,11))
v={k:x*sc for k,x in v.items()}
rows=[("Protezione sociale","Pensioni di vecchiaia",v["GF1002"]),
("Protezione sociale","Altra protezione sociale",v["GF10"]-v["GF1002"]),
("Sanità","Sanità",v["GF07"]),("Affari economici","Affari economici",v["GF04"]),
("Servizi generali","Interessi sul debito",v["GF0107"]),("Servizi generali","Altri servizi generali",v["GF01"]-v["GF0107"]),
("Istruzione","Istruzione",v["GF09"]),("Ordine pubblico","Ordine pubblico\\ne sicurezza",v["GF03"]),
("Ambiente","Ambiente",v["GF05"]),("Cultura","Cultura",v["GF08"]),("Abitazioni","Abitazioni",v["GF06"]),
("Difesa","Difesa: stipendi e spese correnti",v["GF02"]-v["GF02_P51G"]),("Difesa","Difesa: investimenti",v["GF02_P51G"])]
import csv
out="/Users/lorenzoruffino/Documents/Progetti/data-viz/Spesa per la difesa/input/stima_cofog_2025.csv"
with open(out,"w",newline="") as fh:
    w=csv.writer(fh); w.writerow(["gruppo","voce","mld_2025_stima","mld_2024"])
    base={"Pensioni di vecchiaia":e("GF1002","TE"),"Interessi sul debito":e("GF0107","TE"),"Difesa: investimenti":e("GF02","P51G")}
    for gr,vo,x in rows: w.writerow([gr,vo,round(x/1000,1),""])
for gr,vo,x in rows: print(f"{vo:35s} {x/1000:6.1f}  {x/m('TE',2025)*100:5.2f}%")
print("difesa tot", round(v["GF02"]/1000,2), "% Pil", round(v["GF02"]/2265002*100,3), "interessi/difesa", round(v["GF0107"]/v["GF02"],2), "base interessi 2024", e("GF0107","TE"), "pens 2024", e("GF1002","TE"), "inv dif 2024", e("GF02","P51G"))

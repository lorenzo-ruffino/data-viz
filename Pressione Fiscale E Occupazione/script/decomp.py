from ej import load
import os; IN=os.path.join(os.path.dirname(os.path.abspath(__file__)),"..","input","")
_,labs,tx=load(IN+'eurostat_gov_10a_taxag_IT.json'); yrs=labs[-1]
_,_,gd=load(IN+'eurostat_nama_10_gdp_IT_CP.json'); _,_,gr=load(IN+'eurostat_nama_10_gdp_IT_CLV.json'); _,_,em=load(IN+'eurostat_nama_10_a10_e_IT.json'); _,_,gm=load(IN+'eurostat_gov_10a_main_IT_D62.json')
T=lambda it,y: tx.get(('A','MIO_EUR','S13_S212',it,'IT',y),0)/1000
G=lambda it,y: gd.get(('A','CP_MEUR',it,'IT',y),0)/1000
R=lambda it,y: gr.get(('A','CLV20_MEUR',it,'IT',y),0)/1000
H=lambda y: em[('A','THS_HW','TOTAL','SAL_DC','IT',y)]
PEN=lambda y: gm[('A','MIO_EUR','S13','D62PAY','IT',y)]/1000 - (4.4 if y=='2025' else 0)
import sys
A_DIP,A_PEN=0.54,0.31   # quote IRPEF netta MEF (dipendenti, pensionati) ~ a.i. 2023-2024
A_DIP=float(sys.argv[1]) if len(sys.argv)>1 else A_DIP
A_OTH=1-A_DIP-A_PEN
def comps(y):
    tot=T('D2_D5_D91_D61_M_D612_M_D614_M_D995',y)
    c={}
    c['Contributi dipendenti (D613CE)']=(T('D613CE',y),G('D11',y),'L')
    c['Contributi datori (D611)']=(T('D611',y),G('D11',y),'L')
    c['IRPEF&c. su lavoro dip. (D51A)']=(A_DIP*T('D51A',y),G('D11',y),'L')
    c['IRPEF&c. su pensioni (D51A)']=(A_PEN*T('D51A',y),PEN(y),'P')
    c['IRPEF&c. altri redditi (D51A)']=(A_OTH*T('D51A',y),G('B2A3G',y),'K')
    c['Contributi autonomi (D613CS)']=(T('D613CS',y),G('B2A3G',y),'K')
    c['IRES (D51B)']=(T('D51B',y),G('B2A3G',y),'K')
    c['Imposte su plusvalenze (D51C)']=(T('D51C',y),G('B1GQ',y),'O')
    c['IVA (D211)']=(T('D211',y),G('P31_S14',y),'C')
    c['Accise (D214A)']=(T('D214A',y),G('P31_S14',y),'C')
    c['Altre ind. (IRAP,IMU,bollo..)']=(T('D2',y)-T('D211',y)-T('D214A',y),G('B1GQ',y),'O')
    oth=tot-sum(v[0] for v in c.values())
    c['Altro (D5 altre, D91, D61 altri)']=(oth,G('B1GQ',y),'O')
    return tot,c
def step(y0,y1,verbose=True):
    Y0,Y1=G('B1GQ',y0),G('B1GQ',y1)
    t0,c0=comps(y0); t1,c1=comps(y1)
    gY=Y1/Y0
    rows=[];CP=RT=0
    for k in c0:
        a0,b0,cat=c0[k]; a1,b1,_=c1[k]
        d=100*(a1/Y1-a0/Y0)
        comp=100*(a0/Y0)*((b1/b0)/gY-1)
        rows.append((k,cat,100*a0/Y0,d,comp,d-comp))
    return 100*(t1/Y1-t0/Y0),rows
def labsplit(y0,y1):
    # quota lavoro: occupazione (ore dip vs PIL reale) vs salario orario reale (vs deflatore PIL)
    gN=H(y1)/H(y0); gYr=R('B1GQ',y1)/R('B1GQ',y0); gW=(G('D11',y1)/G('D11',y0))/gN; gP=(G('B1GQ',y1)/G('B1GQ',y0))/gYr
    return gN,gYr,gW,gP
if __name__=='__main__':
    for y in yrs: 
        if y>='2019': print(y,'quota D1/PIL %.2f  D11/PIL %.2f  B2A3G/PIL %.2f'%(100*G('D1',y)/G('B1GQ',y),100*G('D11',y)/G('B1GQ',y),100*G('B2A3G',y)/G('B1GQ',y)))
    for (y0,y1) in [('2022','2023'),('2023','2024'),('2024','2025'),('2022','2025'),('2019','2025')]:
        d,rows=step(y0,y1)
        print(f"\n===== {y0}->{y1}: Δ pressione {d:+.2f} pp")
        print(f"{'voce':34s} {'cat':3s} {'liv0':>6s} {'Δ':>6s} {'compos':>7s} {'aliq':>6s}")
        for r in rows: print(f"{r[0]:34s} {r[1]:3s} {r[2]:6.2f} {r[3]:+6.2f} {r[4]:+7.2f} {r[5]:+6.2f}")
        cats={}
        for r in rows:
            cc=cats.setdefault(r[1],[0,0,0]); cc[0]+=r[3]; cc[1]+=r[4]; cc[2]+=r[5]
        for k,v in cats.items(): print(f"  cat {k}: Δ {v[0]:+.2f}  composizione {v[1]:+.2f}  aliquota {v[2]:+.2f}")
        print(f"  TOT composizione {sum(r[4] for r in rows):+.2f}  TOT aliquote/altro {sum(r[5] for r in rows):+.2f}")
        gN,gYr,gW,gP=labsplit(y0,y1)
        print(f"  ore dip {100*(gN-1):+.1f}%  PIL reale {100*(gYr-1):+.1f}%  retrib/ora {100*(gW-1):+.1f}%  deflatore {100*(gP-1):+.1f}%")
        L=[r for r in rows if r[1]=='L']; lev=sum(r[2] for r in L)
        occ=lev*(gN/gYr-1); wag=lev*((gW/gP)-1)
        print(f"  prelievo lavoro dip = {lev:.2f}% PIL; comp. 'occupazione>PIL' {occ:+.2f}, comp. 'salario reale' {wag:+.2f} (lordo, prima di compensazione su K)")

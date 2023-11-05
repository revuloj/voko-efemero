% kio estas Prologo?
prologo(programlingvo).
prologo(solvilo).
prologo(datumbazo).
prologo(serĉilo).

estas(predikato,tabelo).
estas(fakto,datenopo).
estas(fakto,rikordo).
estas(variablo,variablo).
estas(atomo,nomo).
estas(regulo,'Kapo :- Korpo.').
estas(kapo,termo).
estas(korpo,termo).
estas(kapo,predikato).
estas(korpo,esprimo).

/*
select r3kap.kap, r3mrk.mrk, r3trd.lng, r3trd.trd, r3trd.ind from r3trd
left join r3mrk ON r3trd.mrk = r3mrk.mrk
LEFT JOIN r3kap ON r3kap.mrk = r3mrk.drv 
where r3trd.lng in ('en','fr','nl','de') and r3trd.ekz = ''
limit 130;
*/

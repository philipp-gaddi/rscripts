library(survival)
clogoutKat <- clogit(choice~noteKat+anzahlKat+photoID+strata(vpIDScen), data=choice.kat)
print(clogoutKat)

clogoutInst <- clogit(choice~note+anzahl+photoID+strata(vpIDScen), data=choice.inst)
print(clogoutInst)
clogoutInst2 <- clogit(choice~note+anzahl+photoID+male+male:vp_male+locPos+strata(vpIDScen), data=choice.inst)
print(clogoutInst2)


### eval per vpID and categorize results in types
### look the table with chosen categories per trial and evaluate relative response behavior
clogoutInst.vp <- list()
for (vpid in vpids) {
  df <- choice.inst.vp[[vpid]]
  clogoutInst.vp[[vpid]] <- clogit(choice~note+anzahl+photoID+strata(vpIDScen), data=df)
}
remove(df, vpid)

### eval per vpID and categorize results in types
### look the table with chosen categories per trial and evaluate relative response behavior
clogoutInst2.vp <- list()
for (vpid in vpids) {
  df <- choice.inst.vp[[vpid]]
  clogoutInst2.vp[[vpid]] <- clogit(choice~note+anzahl+photoID+male+locPos+strata(vpIDScen), data=df)
}
remove(df, vpid)

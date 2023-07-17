Appendix 1
================
Jacob C. Cooper
2023-07-17

Concatenated here are all additional figures. Please note that some
figures are missing, as the code is written to ‘skip’ figures that are
identity plots or repeated plots. All unique and relevant figures are
shown.

``` r
library(tidyverse)
library(data.table)
library(rasterVis)
library(lattice)
```

# Visualizing differences

``` r
files=list.files(filepath,pattern="*mve_comparisons.csv")

sp.comp.files=files[files%like%"Progne"]
subis.comp.files=files[-which(files%like%"Progne")]
```

## Species comparisons

``` r
for(i in 1:length(sp.comp.files)){
  sp=strsplit(sp.comp.files[i],"_")[[1]][2]
  x1=read_csv(paste0(filepath,sp.comp.files[i]))
  for(ii in 1:ncol(x1)){
    comp.sp=strsplit(colnames(x1)[ii],"_")[[1]][2]
    list.1=x1[,ii]
    
    if(comp.sp==sp){next}
    x2=read_csv(paste0(filepath,sp.comp.files[sp.comp.files%like%comp.sp]))
    list.2=x2[-1,which(colnames(x2)%like%sp)]
    
    list.1$ID=paste0(sp,'_',comp.sp)
    list.2$ID=paste0(comp.sp,'_',sp)
    
    test_val=list.1[1,1]
    list.1=list.1[-1,]
    
    colnames(list.1)=colnames(list.2)=c("Value","ID")
    
    val.list=as.data.frame(rbind(list.1,list.2))
    
    a=ggplot(data=val.list,aes(x=Value,fill=ID))
    b=scale_x_continuous(limits=c(0,1))
    c=geom_density(alpha=0.6)
    d=theme_classic()
    d2=geom_vline(xintercept = as.numeric(test_val),colour="black",
                  linetype="dashed")
    d3=ggtitle(paste0("Figure A.",i,ii))
    
    print(a+b+c+d+d2+d3)
    
    IDs=unique(val.list$ID)
    
    for(iii in 1:2){
      ID=IDs[iii]
      print(ID)
      sub.val=val.list[which(val.list$ID==ID),]
      mu=mean(sub.val$Value)
      sigma=sd(sub.val$Value)
      n=nrow(sub.val)
      
      xbar=as.numeric(test_val)
      
      z=(xbar-mu)/(sigma/sqrt(n))
      
      lowcrit=qnorm(p=0.025,mean=mu,sd=sigma)
      hicrit=qnorm(p=0.975,mean=mu,sd=sigma)
      
      if(is.na(lowcrit)){
        print("ERROR: NA values detected.")
        next
      }
      if(is.na(hicrit)){
        print("ERROR: NA values detected.")
        next
      }
      
      
      print(paste0("P value for ",sp," vs. ",comp.sp,
                   " = ",pnorm(xbar,
                               mean=mu,sd=sigma)))
      
      if(xbar<lowcrit){
        print("Test statistic below low critical value.")
        print(paste0(lowcrit,"; statistic = ",xbar))
        next
      }
      if(xbar>hicrit){
        print("Test statistic above high critical value.")
        print(paste0(hicrit,"; statistic = ",xbar))
      }
    }
  }
}
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## [1] "chalybea_subis"
    ## [1] "P value for chalybea vs. subis = 0.00874160329770293"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.564118732714916; statistic = 0.563313689881235"
    ## [1] "subis_chalybea"
    ## [1] "P value for chalybea vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.449945704057096; statistic = 0.563313689881235"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## [1] "chalybea_cryptoleuca"
    ## [1] "P value for chalybea vs. cryptoleuca = 0.905112084577193"
    ## [1] "cryptoleuca_chalybea"
    ## [1] "P value for chalybea vs. cryptoleuca = 0.923949851048234"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

    ## [1] "chalybea_dominicensis"
    ## [1] "P value for chalybea vs. dominicensis = 0.630975743510612"
    ## [1] "dominicensis_chalybea"
    ## [1] "P value for chalybea vs. dominicensis = 0.822508483083474"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

    ## [1] "chalybea_sinaloae"
    ## [1] "P value for chalybea vs. sinaloae = 0.000142331531800325"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.765954690487922; statistic = 0.753219351057136"
    ## [1] "sinaloae_chalybea"
    ## [1] "P value for chalybea vs. sinaloae = 0.790200660181079"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

    ## [1] "chalybea_elegans"
    ## [1] "P value for chalybea vs. elegans = 1.1268673669308e-10"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.608673933067226; statistic = 0.600536008828745"
    ## [1] "elegans_chalybea"
    ## [1] "P value for chalybea vs. elegans = 3.06799382017121e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.723481951512536; statistic = 0.600536008828745"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

    ## [1] "chalybea_murphyi"
    ## [1] "P value for chalybea vs. murphyi = 2.63731972570716e-15"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.616780352612352; statistic = 0.60239972901452"
    ## [1] "murphyi_chalybea"
    ## [1] "P value for chalybea vs. murphyi = 0.00437438262692634"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.637154158297721; statistic = 0.60239972901452"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

    ## [1] "chalybea_modesta"
    ## [1] "P value for chalybea vs. modesta = 0.389634581657439"
    ## [1] "modesta_chalybea"
    ## [1] "P value for chalybea vs. modesta = 0.294789673096894"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

    ## [1] "chalybea_tapera"
    ## [1] "P value for chalybea vs. tapera = 0.972516514513643"
    ## [1] "tapera_chalybea"
    ## [1] "P value for chalybea vs. tapera = 0.886159914910166"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->

    ## [1] "cryptoleuca_subis"
    ## [1] "P value for cryptoleuca vs. subis = 0.000451736307097325"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.568482313562568; statistic = 0.558444925984436"
    ## [1] "subis_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.446182291694113; statistic = 0.558444925984436"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->

    ## [1] "cryptoleuca_dominicensis"
    ## [1] "P value for cryptoleuca vs. dominicensis = 0.779539496090278"
    ## [1] "dominicensis_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. dominicensis = 0.369108677163184"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->

    ## [1] "cryptoleuca_sinaloae"
    ## [1] "P value for cryptoleuca vs. sinaloae = 0.013094146575736"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.791481323974456; statistic = 0.788410073141191"
    ## [1] "sinaloae_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. sinaloae = 0.854437149651724"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->

    ## [1] "cryptoleuca_chalybea"
    ## [1] "P value for cryptoleuca vs. chalybea = 0.923949851048234"
    ## [1] "chalybea_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. chalybea = 0.905112084577193"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->

    ## [1] "cryptoleuca_elegans"
    ## [1] "P value for cryptoleuca vs. elegans = 0.000616333242654848"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.611546019196136; statistic = 0.600134558565549"
    ## [1] "elegans_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. elegans = 4.8983487808998e-26"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.717360041838917; statistic = 0.600134558565549"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->

    ## [1] "cryptoleuca_murphyi"
    ## [1] "P value for cryptoleuca vs. murphyi = 0.000307285776401752"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.629372014246327; statistic = 0.617227887259557"
    ## [1] "murphyi_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. murphyi = 0.0109638774769042"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.636418105864818; statistic = 0.617227887259557"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->

    ## [1] "cryptoleuca_modesta"
    ## [1] "P value for cryptoleuca vs. modesta = 0.0920961505582708"
    ## [1] "modesta_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. modesta = 0.251702770103792"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->

    ## [1] "cryptoleuca_tapera"
    ## [1] "P value for cryptoleuca vs. tapera = 0.793627816390721"
    ## [1] "tapera_cryptoleuca"
    ## [1] "P value for cryptoleuca vs. tapera = 0.454750151764007"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->

    ## [1] "dominicensis_subis"
    ## [1] "P value for dominicensis vs. subis = 0.541592052165962"
    ## [1] "subis_dominicensis"
    ## [1] "P value for dominicensis vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.458350885803299; statistic = 0.546931108217863"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->

    ## [1] "dominicensis_cryptoleuca"
    ## [1] "P value for dominicensis vs. cryptoleuca = 0.369108677163184"
    ## [1] "cryptoleuca_dominicensis"
    ## [1] "P value for dominicensis vs. cryptoleuca = 0.779539496090278"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->

    ## [1] "dominicensis_sinaloae"
    ## [1] "P value for dominicensis vs. sinaloae = 0.107596749406277"
    ## [1] "sinaloae_dominicensis"
    ## [1] "P value for dominicensis vs. sinaloae = 0.873365587875063"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->

    ## [1] "dominicensis_chalybea"
    ## [1] "P value for dominicensis vs. chalybea = 0.822508483083474"
    ## [1] "chalybea_dominicensis"
    ## [1] "P value for dominicensis vs. chalybea = 0.630975743510612"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->

    ## [1] "dominicensis_elegans"
    ## [1] "P value for dominicensis vs. elegans = 0.234480173997123"
    ## [1] "elegans_dominicensis"
    ## [1] "P value for dominicensis vs. elegans = 2.41081677418463e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.665239752965937; statistic = 0.564055703589358"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->

    ## [1] "dominicensis_murphyi"
    ## [1] "P value for dominicensis vs. murphyi = 0.269514182507919"
    ## [1] "murphyi_dominicensis"
    ## [1] "P value for dominicensis vs. murphyi = 0.0192757153826417"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.61194638994795; statistic = 0.603802914312001"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->

    ## [1] "dominicensis_modesta"
    ## [1] "P value for dominicensis vs. modesta = 0.912132791878"
    ## [1] "modesta_dominicensis"
    ## [1] "P value for dominicensis vs. modesta = 0.560153596534554"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->

    ## [1] "dominicensis_tapera"
    ## [1] "P value for dominicensis vs. tapera = 0.53343858396198"
    ## [1] "tapera_dominicensis"
    ## [1] "P value for dominicensis vs. tapera = 0.000351440909227227"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.83855636419014; statistic = 0.821881855815174"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-25.png)<!-- -->

    ## [1] "elegans_subis"
    ## [1] "P value for elegans vs. subis = 0.999999380362749"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.808541951133876; statistic = 0.873820627396604"
    ## [1] "subis_elegans"
    ## [1] "P value for elegans vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.758251033354595; statistic = 0.873820627396604"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-26.png)<!-- -->

    ## [1] "elegans_cryptoleuca"
    ## [1] "P value for elegans vs. cryptoleuca = 4.8983487808998e-26"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.717360041838917; statistic = 0.600134558565549"
    ## [1] "cryptoleuca_elegans"
    ## [1] "P value for elegans vs. cryptoleuca = 0.000616333242654848"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.611546019196136; statistic = 0.600134558565549"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-27.png)<!-- -->

    ## [1] "elegans_dominicensis"
    ## [1] "P value for elegans vs. dominicensis = 2.41081677418463e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.665239752965937; statistic = 0.564055703589358"
    ## [1] "dominicensis_elegans"
    ## [1] "P value for elegans vs. dominicensis = 0.234480173997123"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-28.png)<!-- -->

    ## [1] "elegans_sinaloae"
    ## [1] "P value for elegans vs. sinaloae = 1.7424162547499e-27"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.821080281493838; statistic = 0.738253600383395"
    ## [1] "sinaloae_elegans"
    ## [1] "P value for elegans vs. sinaloae = 0.597446716031685"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-29.png)<!-- -->

    ## [1] "elegans_chalybea"
    ## [1] "P value for elegans vs. chalybea = 3.06799382017121e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.723481951512536; statistic = 0.600536008828745"
    ## [1] "chalybea_elegans"
    ## [1] "P value for elegans vs. chalybea = 1.1268673669308e-10"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.608673933067226; statistic = 0.600536008828745"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-30.png)<!-- -->

    ## [1] "elegans_murphyi"
    ## [1] "P value for elegans vs. murphyi = 0.999987304433576"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.85663781619806; statistic = 0.901990509419277"
    ## [1] "murphyi_elegans"
    ## [1] "P value for elegans vs. murphyi = 0.961251333736766"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-31.png)<!-- -->

    ## [1] "elegans_modesta"
    ## [1] "P value for elegans vs. modesta = 6.59915408259064e-47"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.66486893233368; statistic = 0.600700504205202"
    ## [1] "modesta_elegans"
    ## [1] "P value for elegans vs. modesta = 0.22031065896722"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-32.png)<!-- -->

    ## [1] "elegans_tapera"
    ## [1] "P value for elegans vs. tapera = 2.28619337933981e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.754411253629397; statistic = 0.63014437319718"
    ## [1] "tapera_elegans"
    ## [1] "P value for elegans vs. tapera = 0.999999492809445"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.620340813315704; statistic = 0.63014437319718"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-33.png)<!-- -->

    ## [1] "modesta_subis"
    ## [1] "P value for modesta vs. subis = 0.1078831536929"
    ## [1] "subis_modesta"
    ## [1] "P value for modesta vs. subis = 0.999999988800046"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.540528555485547; statistic = 0.583264877616411"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-34.png)<!-- -->

    ## [1] "modesta_cryptoleuca"
    ## [1] "P value for modesta vs. cryptoleuca = 0.251702770103792"
    ## [1] "cryptoleuca_modesta"
    ## [1] "P value for modesta vs. cryptoleuca = 0.0920961505582708"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-35.png)<!-- -->

    ## [1] "modesta_dominicensis"
    ## [1] "P value for modesta vs. dominicensis = 0.560153596534548"
    ## [1] "dominicensis_modesta"
    ## [1] "P value for modesta vs. dominicensis = 0.912132791877994"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-36.png)<!-- -->

    ## [1] "modesta_sinaloae"
    ## [1] "P value for modesta vs. sinaloae = 0.333801326243559"
    ## [1] "sinaloae_modesta"
    ## [1] "P value for modesta vs. sinaloae = 0.374690294593921"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-37.png)<!-- -->

    ## [1] "modesta_chalybea"
    ## [1] "P value for modesta vs. chalybea = 0.294789673096891"
    ## [1] "chalybea_modesta"
    ## [1] "P value for modesta vs. chalybea = 0.389634581657429"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-38.png)<!-- -->

    ## [1] "modesta_elegans"
    ## [1] "P value for modesta vs. elegans = 0.220310658967219"
    ## [1] "elegans_modesta"
    ## [1] "P value for modesta vs. elegans = 6.59915408258659e-47"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.66486893233368; statistic = 0.600700504205202"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-39.png)<!-- -->

    ## [1] "modesta_murphyi"
    ## [1] "P value for modesta vs. murphyi = 0.306293889036427"
    ## [1] "murphyi_modesta"
    ## [1] "P value for modesta vs. murphyi = 0.0125321881521672"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.648027960567164; statistic = 0.62897231131383"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-40.png)<!-- -->

    ## [1] "modesta_tapera"
    ## [1] "P value for modesta vs. tapera = 0.233956062658453"
    ## [1] "tapera_modesta"
    ## [1] "P value for modesta vs. tapera = 0.000167011894132902"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.712997150805312; statistic = 0.68199189109927"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-41.png)<!-- -->

    ## [1] "murphyi_subis"
    ## [1] "P value for murphyi vs. subis = 0.971139295288171"
    ## [1] "subis_murphyi"
    ## [1] "P value for murphyi vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.758298132126779; statistic = 0.835123293713782"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-42.png)<!-- -->

    ## [1] "murphyi_cryptoleuca"
    ## [1] "P value for murphyi vs. cryptoleuca = 0.00765410075705067"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.636418105864818; statistic = 0.609509779095934"
    ## [1] "cryptoleuca_murphyi"
    ## [1] "P value for murphyi vs. cryptoleuca = 6.61526226502395e-06"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.629372014246327; statistic = 0.609509779095934"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-43.png)<!-- -->

    ## [1] "murphyi_dominicensis"
    ## [1] "P value for murphyi vs. dominicensis = 0.017876444789363"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.61194638994795; statistic = 0.601502602342795"
    ## [1] "dominicensis_murphyi"
    ## [1] "P value for murphyi vs. dominicensis = 0.204464259340592"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-44.png)<!-- -->

    ## [1] "murphyi_sinaloae"
    ## [1] "P value for murphyi vs. sinaloae = 0.249887811838028"
    ## [1] "sinaloae_murphyi"
    ## [1] "P value for murphyi vs. sinaloae = 0.76773496956385"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-45.png)<!-- -->

    ## [1] "murphyi_chalybea"
    ## [1] "P value for murphyi vs. chalybea = 0.00216732497679625"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.637154158297721; statistic = 0.590267738596306"
    ## [1] "chalybea_murphyi"
    ## [1] "P value for murphyi vs. chalybea = 1.30125236540328e-37"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.616780352612352; statistic = 0.590267738596306"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-46.png)<!-- -->

    ## [1] "murphyi_elegans"
    ## [1] "P value for murphyi vs. elegans = 0.953606783256938"
    ## [1] "elegans_murphyi"
    ## [1] "P value for murphyi vs. elegans = 0.999931943900692"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.85663781619806; statistic = 0.894009802454622"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-47.png)<!-- -->

    ## [1] "murphyi_modesta"
    ## [1] "P value for murphyi vs. modesta = 0.0269308162178768"
    ## [1] "modesta_murphyi"
    ## [1] "P value for murphyi vs. modesta = 0.459275587048333"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-48.png)<!-- -->

    ## [1] "murphyi_tapera"
    ## [1] "P value for murphyi vs. tapera = 0.00388454823008583"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.649649760588276; statistic = 0.615724242493323"
    ## [1] "tapera_murphyi"
    ## [1] "P value for murphyi vs. tapera = 0.923749328771478"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-49.png)<!-- -->

    ## [1] "sinaloae_subis"
    ## [1] "P value for sinaloae vs. subis = 0.573177191904059"
    ## [1] "subis_sinaloae"
    ## [1] "P value for sinaloae vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.580979920109348; statistic = 0.687111531570226"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-50.png)<!-- -->

    ## [1] "sinaloae_cryptoleuca"
    ## [1] "P value for sinaloae vs. cryptoleuca = 0.858123249895697"
    ## [1] "cryptoleuca_sinaloae"
    ## [1] "P value for sinaloae vs. cryptoleuca = 0.0178629901775483"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.791481323974456; statistic = 0.789848394763692"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-51.png)<!-- -->

    ## [1] "sinaloae_dominicensis"
    ## [1] "P value for sinaloae vs. dominicensis = 0.883512971775754"
    ## [1] "dominicensis_sinaloae"
    ## [1] "P value for sinaloae vs. dominicensis = 0.168545748778204"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-52.png)<!-- -->

    ## [1] "sinaloae_chalybea"
    ## [1] "P value for sinaloae vs. chalybea = 0.77871818448255"
    ## [1] "chalybea_sinaloae"
    ## [1] "P value for sinaloae vs. chalybea = 2.58592319317008e-05"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.765954690487922; statistic = 0.750023122900644"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-53.png)<!-- -->

    ## [1] "sinaloae_elegans"
    ## [1] "P value for sinaloae vs. elegans = 0.595819427529082"
    ## [1] "elegans_sinaloae"
    ## [1] "P value for sinaloae vs. elegans = 1.07682315635689e-27"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.821080281493838; statistic = 0.737840312619779"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-54.png)<!-- -->

    ## [1] "sinaloae_murphyi"
    ## [1] "P value for sinaloae vs. murphyi = 0.76773496956385"
    ## [1] "murphyi_sinaloae"
    ## [1] "P value for sinaloae vs. murphyi = 0.249887811838028"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-55.png)<!-- -->

    ## [1] "sinaloae_modesta"
    ## [1] "P value for sinaloae vs. modesta = 0.549559407515952"
    ## [1] "modesta_sinaloae"
    ## [1] "P value for sinaloae vs. modesta = 0.522509267513247"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-56.png)<!-- -->

    ## [1] "sinaloae_tapera"
    ## [1] "P value for sinaloae vs. tapera = 0.837385351802245"
    ## [1] "tapera_sinaloae"
    ## [1] "P value for sinaloae vs. tapera = 0.188657928500043"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-57.png)<!-- -->

    ## [1] "subis_cryptoleuca"
    ## [1] "P value for subis vs. cryptoleuca = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.446182291694113; statistic = 0.556518426277204"
    ## [1] "cryptoleuca_subis"
    ## [1] "P value for subis vs. cryptoleuca = 0.000171916898040372"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.568482313562568; statistic = 0.556518426277204"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-58.png)<!-- -->

    ## [1] "subis_dominicensis"
    ## [1] "P value for subis vs. dominicensis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.458350885803299; statistic = 0.551734842020407"
    ## [1] "dominicensis_subis"
    ## [1] "P value for subis vs. dominicensis = 0.687438381466892"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-59.png)<!-- -->

    ## [1] "subis_sinaloae"
    ## [1] "P value for subis vs. sinaloae = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.580979920109348; statistic = 0.687111531570226"
    ## [1] "sinaloae_subis"
    ## [1] "P value for subis vs. sinaloae = 0.573177191904059"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-60.png)<!-- -->

    ## [1] "subis_chalybea"
    ## [1] "P value for subis vs. chalybea = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.449945704057096; statistic = 0.557892897787648"
    ## [1] "chalybea_subis"
    ## [1] "P value for subis vs. chalybea = 1.10720012896822e-07"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.564118732714916; statistic = 0.557892897787648"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-61.png)<!-- -->

    ## [1] "subis_elegans"
    ## [1] "P value for subis vs. elegans = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.758251033354595; statistic = 0.870619103076361"
    ## [1] "elegans_subis"
    ## [1] "P value for subis vs. elegans = 0.999998746444586"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.808541951133876; statistic = 0.870619103076361"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-62.png)<!-- -->

    ## [1] "subis_murphyi"
    ## [1] "P value for subis vs. murphyi = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.758298132126779; statistic = 0.835123293713782"
    ## [1] "murphyi_subis"
    ## [1] "P value for subis vs. murphyi = 0.971139295288171"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-63.png)<!-- -->

    ## [1] "subis_modesta"
    ## [1] "P value for subis vs. modesta = 0.999999999998774"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.540528555485547; statistic = 0.599896377154647"
    ## [1] "modesta_subis"
    ## [1] "P value for subis vs. modesta = 0.181223380709725"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-64.png)<!-- -->

    ## [1] "subis_tapera"
    ## [1] "P value for subis vs. tapera = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.478120915348566; statistic = 0.588251057285956"
    ## [1] "tapera_subis"
    ## [1] "P value for subis vs. tapera = 0.999999999999996"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.570620486311015; statistic = 0.588251057285956"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-65.png)<!-- -->

    ## [1] "tapera_subis"
    ## [1] "P value for tapera vs. subis = 0.999999999999996"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.570620486311015; statistic = 0.588251057285956"
    ## [1] "subis_tapera"
    ## [1] "P value for tapera vs. subis = 1"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.478120915348566; statistic = 0.588251057285956"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-66.png)<!-- -->

    ## [1] "tapera_cryptoleuca"
    ## [1] "P value for tapera vs. cryptoleuca = 0.881938628576874"
    ## [1] "cryptoleuca_tapera"
    ## [1] "P value for tapera vs. cryptoleuca = 0.953802915264647"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-67.png)<!-- -->

    ## [1] "tapera_dominicensis"
    ## [1] "P value for tapera vs. dominicensis = 0.00967468212857087"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.83855636419014; statistic = 0.834135081207135"
    ## [1] "dominicensis_tapera"
    ## [1] "P value for tapera vs. dominicensis = 0.778955908690357"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-68.png)<!-- -->

    ## [1] "tapera_sinaloae"
    ## [1] "P value for tapera vs. sinaloae = 0.188657928500043"
    ## [1] "sinaloae_tapera"
    ## [1] "P value for tapera vs. sinaloae = 0.837385351802245"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-69.png)<!-- -->

    ## [1] "tapera_chalybea"
    ## [1] "P value for tapera vs. chalybea = 0.909860928781084"
    ## [1] "chalybea_tapera"
    ## [1] "P value for tapera vs. chalybea = 0.980030480793911"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.942700177395444; statistic = 0.943446311489337"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-70.png)<!-- -->

    ## [1] "tapera_elegans"
    ## [1] "P value for tapera vs. elegans = 0.999999195787371"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.620340813315704; statistic = 0.629837909273023"
    ## [1] "elegans_tapera"
    ## [1] "P value for tapera vs. elegans = 1.78641501617912e-28"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.754411253629397; statistic = 0.629837909273023"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-71.png)<!-- -->

    ## [1] "tapera_murphyi"
    ## [1] "P value for tapera vs. murphyi = 0.923749328771478"
    ## [1] "murphyi_tapera"
    ## [1] "P value for tapera vs. murphyi = 0.00388454823008583"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.649649760588276; statistic = 0.615724242493323"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-4-72.png)<!-- -->

    ## [1] "tapera_modesta"
    ## [1] "P value for tapera vs. modesta = 0.0960970246071175"
    ## [1] "modesta_tapera"
    ## [1] "P value for tapera vs. modesta = 0.535747443004034"

## Subspecies comparisons

``` r
for(i in 1:length(subis.comp.files)){
  sp=strsplit(subis.comp.files[i],"_")[[1]][1]
  x1=read_csv(paste0(filepath,subis.comp.files[i]))
  for(ii in 1:ncol(x1)){
    comp.sp=strsplit(colnames(x1)[ii],"_")[[1]][3]
    list.1=x1[,ii]
    
    if(comp.sp==sp){next}
    x2=read_csv(paste0(filepath,subis.comp.files[subis.comp.files%like%comp.sp]))
    list.2=x2[-1,which(colnames(x2)%like%sp)]
    
    list.1$ID=paste0(sp,'_',comp.sp)
    list.2$ID=paste0(comp.sp,'_',sp)
    
    test_val=list.1[1,1]
    list.1=list.1[-1,]
    
    colnames(list.1)=colnames(list.2)=c("Value","ID")
    
    val.list=as.data.frame(rbind(list.1,list.2))
    
    a=ggplot(data=val.list,aes(x=Value,fill=ID))
    b=scale_x_continuous(limits=c(0,1))
    c=geom_density(alpha=0.6)
    d=theme_classic()
    d2=geom_vline(xintercept = as.numeric(test_val),colour="black",
                  linetype="dashed")
    d3=ggtitle(paste0("Figure B.",i,ii))
    
    print(a+b+c+d+d2+d3)
    
    IDs=unique(val.list$ID)
    
    for(iii in 1:2){
      ID=IDs[iii]
      print(ID)
      sub.val=val.list[which(val.list$ID==ID),]
      mu=mean(sub.val$Value)
      sigma=sd(sub.val$Value)
      n=nrow(sub.val)
      
      xbar=as.numeric(test_val)
      
      z=(xbar-mu)/(sigma/sqrt(n))
      
      lowcrit=qnorm(p=0.025,mean=mu,sd=sigma)
      hicrit=qnorm(p=0.975,mean=mu,sd=sigma)
      
      print(paste0("P value for ",sp," vs. ",comp.sp,
                   " = ",pnorm(xbar,
                               mean=mu,sd=sigma)))
      
      if(xbar<lowcrit){
        print("Test statistic below low critical value.")
        print(paste0(lowcrit,"; statistic = ",xbar))
        next
      }
      if(xbar>hicrit){
        print("Test statistic above high critical value.")
        print(paste0(hicrit,"; statistic = ",xbar))
        next
      }
    }
  }
}
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## [1] "arboricola_subis"
    ## [1] "P value for arboricola vs. subis = 0.897214018750521"
    ## [1] "subis_arboricola"
    ## [1] "P value for arboricola vs. subis = 0.801150285765002"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## [1] "arboricola_hesperia"
    ## [1] "P value for arboricola vs. hesperia = 0.958304222864935"
    ## [1] "hesperia_arboricola"
    ## [1] "P value for arboricola vs. hesperia = 0.951036130670033"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

    ## [1] "arboricola_pacifica"
    ## [1] "P value for arboricola vs. pacifica = 0.995921993929447"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.684491016861398; statistic = 0.692175835838449"
    ## [1] "pacifica_arboricola"
    ## [1] "P value for arboricola vs. pacifica = 0.98656810582395"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.691532025784251; statistic = 0.692175835838449"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

    ## [1] "arboricola_enigmae"
    ## [1] "P value for arboricola vs. enigmae = 0.605488596325206"
    ## [1] "enigmae_arboricola"
    ## [1] "P value for arboricola vs. enigmae = 0.000125633706492038"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.732169888846646; statistic = 0.696818752956035"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

    ## [1] "enigmae_subis"
    ## [1] "P value for enigmae vs. subis = 0.643415185715688"
    ## [1] "subis_enigmae"
    ## [1] "P value for enigmae vs. subis = 0.805847702903804"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

    ## [1] "enigmae_arboricola"
    ## [1] "P value for enigmae vs. arboricola = 0.000125633706492038"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.732169888846646; statistic = 0.696818752956035"
    ## [1] "arboricola_enigmae"
    ## [1] "P value for enigmae vs. arboricola = 0.605488596325206"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->

    ## [1] "enigmae_hesperia"
    ## [1] "P value for enigmae vs. hesperia = 0.000235903781827756"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.804383230678103; statistic = 0.762675562886749"
    ## [1] "hesperia_enigmae"
    ## [1] "P value for enigmae vs. hesperia = 0.945650775417108"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->

    ## [1] "enigmae_pacifica"
    ## [1] "P value for enigmae vs. pacifica = 0.0750848771079954"
    ## [1] "pacifica_enigmae"
    ## [1] "P value for enigmae vs. pacifica = 0.999118403218665"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.656257251372496; statistic = 0.659932933213162"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->

    ## [1] "hesperia_subis"
    ## [1] "P value for hesperia vs. subis = 0.963250045671483"
    ## [1] "subis_hesperia"
    ## [1] "P value for hesperia vs. subis = 0.999999562151269"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.675132144914459; statistic = 0.702759858381477"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->

    ## [1] "hesperia_arboricola"
    ## [1] "P value for hesperia vs. arboricola = 0.951036130670033"
    ## [1] "arboricola_hesperia"
    ## [1] "P value for hesperia vs. arboricola = 0.958304222864935"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->

    ## [1] "hesperia_pacifica"
    ## [1] "P value for hesperia vs. pacifica = 0.985861219536583"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.723152555455607; statistic = 0.732404918264699"
    ## [1] "pacifica_hesperia"
    ## [1] "P value for hesperia vs. pacifica = 0.999810707015926"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.729138438790516; statistic = 0.732404918264699"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-12.png)<!-- -->

    ## [1] "hesperia_enigmae"
    ## [1] "P value for hesperia vs. enigmae = 0.945650775417108"
    ## [1] "enigmae_hesperia"
    ## [1] "P value for hesperia vs. enigmae = 0.000235903781827756"
    ## [1] "Test statistic below low critical value."
    ## [1] "0.804383230678103; statistic = 0.762675562886749"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-13.png)<!-- -->

    ## [1] "pacifica_subis"
    ## [1] "P value for pacifica vs. subis = 0.923700723743345"
    ## [1] "subis_pacifica"
    ## [1] "P value for pacifica vs. subis = 0.99999999961522"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.625228933535226; statistic = 0.649373031253288"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-14.png)<!-- -->

    ## [1] "pacifica_arboricola"
    ## [1] "P value for pacifica vs. arboricola = 0.98656810582395"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.691532025784251; statistic = 0.692175835838449"
    ## [1] "arboricola_pacifica"
    ## [1] "P value for pacifica vs. arboricola = 0.995921993929447"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.684491016861398; statistic = 0.692175835838449"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-15.png)<!-- -->

    ## [1] "pacifica_hesperia"
    ## [1] "P value for pacifica vs. hesperia = 0.999810707015926"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.729138438790516; statistic = 0.732404918264699"
    ## [1] "hesperia_pacifica"
    ## [1] "P value for pacifica vs. hesperia = 0.985861219536583"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.723152555455607; statistic = 0.732404918264699"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-16.png)<!-- -->

    ## [1] "pacifica_enigmae"
    ## [1] "P value for pacifica vs. enigmae = 0.999118403218665"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.656257251372496; statistic = 0.659932933213162"
    ## [1] "enigmae_pacifica"
    ## [1] "P value for pacifica vs. enigmae = 0.0750848771079954"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-17.png)<!-- -->

    ## [1] "subis_arboricola"
    ## [1] "P value for subis vs. arboricola = 0.801150285765002"
    ## [1] "arboricola_subis"
    ## [1] "P value for subis vs. arboricola = 0.897214018750521"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-18.png)<!-- -->

    ## [1] "subis_hesperia"
    ## [1] "P value for subis vs. hesperia = 0.999999562151269"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.675132144914459; statistic = 0.702759858381477"
    ## [1] "hesperia_subis"
    ## [1] "P value for subis vs. hesperia = 0.963250045671483"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-19.png)<!-- -->

    ## [1] "subis_pacifica"
    ## [1] "P value for subis vs. pacifica = 0.99999999961522"
    ## [1] "Test statistic above high critical value."
    ## [1] "0.625228933535226; statistic = 0.649373031253288"
    ## [1] "pacifica_subis"
    ## [1] "P value for subis vs. pacifica = 0.923700723743345"

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-5-20.png)<!-- -->

    ## [1] "subis_enigmae"
    ## [1] "P value for subis vs. enigmae = 0.805847702903804"
    ## [1] "enigmae_subis"
    ## [1] "P value for subis vs. enigmae = 0.643415185715688"

Running the NicheEvol program in progne.

``` r
filepath="~/Dropbox/Manuscripts/Progne/"
```

``` r
library(geiger)
library(nichevol)
library(raster)
library(tidyverse)
library(data.table)
library(nodiv)

# load rformat_type
library(ellipsenm)
```

# Prep data layers

``` r
# test with one
precip=raster(paste0(GISpath,"current_2-5arcmin_annualPET.tif"))

m_files=list.files(paste0(filepath,"Ms/"),pattern="*accessible.gpkg")

occ_list=read_csv(paste0(filepath,"progne_env_extract.csv"))

p.tree=read.tree(paste0(filepath,"Sequences/all-v2-samples-95-best.raxml.bestTree"))
```

``` r
# fix tree
fnames=p.tree$tip.label

# reduce to Progne
fnames_index=which(!fnames%like%"Progne")

p.tree2=drop.tip(p.tree,fnames_index)
```

``` r
# plot of all Progne
plot.phylo(p.tree2,cex=0.65,main="Figure C.1: All Progne")
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

This tree is from Clare Brown’s dissertation, as made available by Subir
Shakya (with thanks also to Fred Sheldon). From Clare Brown’s
dissertation, we know that *chalybea* is monophyletic, and that it is
not sister to *elegans*. We want to reduce this to a single individual
per species for our next analyses.

``` r
keepers=c("Progne_tapera_tapera_Peru_LSUMNS_75925",
          "Progne_chalybea_Central_America_Panama_LSUMNS_28811",
          "Progne_sinaloae_Mexico_KU_40044",
          "Progne_dominicensis_Dominican_Republic_LSUMNS_22019",
          # "AY825945.1-Progne_cryptoleuca", # not included in phylo
          "Progne_elegans_Argentina_KU_124008", 
          "Progne_subis_USA_LSUMNS_41548",
          "Progne_murphyi_Peru_LSUMZ_114185")#,
          # "MH307578.1-Progne_modesta") # not included in phylo

p.tree3=drop.tip(p.tree2,c(which(p.tree2$tip.label%in%keepers==F)))
plot(p.tree3,main='Figure C.3')  
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# rename tips to SP only
for(i in 1:length(p.tree3$tip.label)){
  sp=strsplit(p.tree3$tip.label[i],"_")[[1]][2]
  #sp=gsub("_"," ",sp)
  p.tree3$tip.label[i]=sp
}

# reformatted other files to be the same name elsewhere
```

``` r
# create ultrametric trees
phy_tree=chronos(p.tree3)
```

    ## 
    ## Setting initial dates...
    ## Fitting in progress... get a first set of estimates
    ##          (Penalised) log-lik = -0.3001499 
    ## Optimising rates... dates... -0.3001499 
    ## 
    ## log-Lik = -0.2990212 
    ## PHIIC = 34.6

``` r
# make edges nonzero
# step not necessary
#p.tree4$edge.length=p.tree4$edge.length+
#  0.00000000000000000000000001
```

While not included in this dataset, we can also place some species from
Moyle et al. (2008), and use these estimates to help in the
reconstruction.

1.  *P. cryptoleuca* is sister to *P. dominicensis*.
2.  There is no molecular evidence for the placement of *P. modesta*.

``` r
write.tree(phy_tree,file=paste0(filepath,"new_tree.tre"))
```

Added taxon in file.

``` r
phy_tree=read.tree(file=paste0(filepath,"new_tree2.tre"))
plot(phy_tree)
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Occurrences and Environmental Data

We need to split occurrences into separate files, and put them in a
separate folder.

``` r
# split occ_list into files named the same as the tips

spp=unique(occ_list$Species)

for(i in 1:length(spp)){
  sp=spp[i]
  occ_list%>%
    filter(Species==sp)%>%
    write_csv(file=paste0(filepath,"occ_dir/",
                          gsub(" ","_",sp),".csv"))
}
```

There is an error in the code. I removed it and then it was able to run.
Specifically, in line 86, I removed reference to the “species”
corresponding to that iteration. Note also that the files in each have
to be an exact match.

``` r
source(paste0(filepath,'stats_evalues2.r'))
```

``` r
stats_evalues2(stats=c("mean","median","sd","range"),
              M_folder=paste0(filepath,"Ms/name_species_tree/"),M_format="gpkg",
              occ_folder=paste0(filepath,"occ_dir/"),
              longitude = "Long",latitude = "Lat",
              var_folder=GIS,var_format="GTiff", # must specify GTiff
              save=T,output_directory = paste0(filepath,"nichevol/"),
              overwrite = T)
```

Testing the next section with annual PET.

``` r
# load both files for annualPET
progne.stats=
  read_csv(paste0(filepath,
                  "nichevol/current_2.5arcmin_annualPET_M_stats.csv"))%>%
  filter(Species!="Progne modesta.gpkg")
m.stats=
  read_csv(paste0(filepath,
                  "nichevol/current_2.5arcmin_annualPET_Occurrence_stats.csv"))%>%
  filter(Species!="Progne modesta.gpkg")
```

These values may vary, as the document renders them each time.

``` r
bins=bintables_jc(M_folder=paste0(filepath,"Ms/name_species_tree/"),
                 M_format="gpkg",occ_folder=paste0(filepath,"occ_dir/"),
                 longitude = "Long",latitude = "Lat",
                 percentage_out = 0,bin_size = 10,
                 var_folder=GIS,var_format="GTiff") # must specify GTiff
```

Link the tree to the data, and then iteratively perform the
reconstructions.

``` r
### loop through and create tree projects

phy_tree2=phy_tree
phy_tree2$tip.label=c("Progne tapera","Progne subis",
                      "Progne elegans","Progne dominicensis",
                      "Progne cryptoleuca","Progne sinaloae",
                      "Progne chalybea","Progne murphyi")

for(i in 1:length(bins)){
  # link tree and data from list object
  var=strsplit(names(bins[i]),"_")[[1]][3]
  if(var=="annualPET"){var="Annual Potential Evapotranspiration"}
  if(var=='aridityIndexThornthwaite'){var='Aridity Index'}
  if(var=='climaticMoistureIndex'){var='Climatic Moisture Index'}
  if(var=='continentality'){var='Continentality'}
  if(var=='embergerQ'){var="Emberger's Pluviometric Quotient"}
  if(var=='maxTempColdest'){var='Max. Temp. of the Coldest Quarter'}
  if(var=='minTempWarmest'){var='Min. Temp. of the Warmest Quarter'}
  if(var=='PETColdestQuarter'){var='Coldest Quarter Potential Evapotranspiration'}
  if(var=='PETDriestQuarter'){var='Driest Quarter Potential Evapotranspiration'}
  if(var=='PETseasonality'){var='Seasonality of Potential Evapotranspiration'}
  if(var=='PETWarmestQuarter'){var='Warmest Quarter Potential Evapotranspiration'}
  if(var=='PETWettestQuarter'){var='Wettest Quarter Potential Evapotranspiration'}
  if(var=='thermicityIndex'){var='Thermicity Index'}
  varproj=treedata(phy_tree2,bins[[i]]) # convert chronos to phylo???
  # create tables for reconstructions
  ml_rec_table=bin_ml_rec(varproj)
  s_ml_rec_table=smooth_rec(ml_rec_table)
  
  base=basal_node(phy_tree2)
  
  var.lab.1=paste0('Figure D.',i,'A: ',var)
  var.lab.2=paste0('Figure D.',i,'B: ',var)
  
  plot.phylo(phy_tree2,label.offset = 0.06,main=var.lab.1,x.lim=4,y.lim=9)
  nichevol_labels(phy_tree2, s_ml_rec_table, height = 1.25, width = 3)
  niche_labels(phy_tree2,s_ml_rec_table,
               label_type = 'tip_node',height=1.25, width = 6)
  nichevol_legend(position = "bottomright", cex = 1)
  
  print("Starting LevelPlot...")
  
  # adapted from Fernando Machado-Stredel
  bin_tabl_tr=t(s_ml_rec_table)
  bin_tabl_tr[bin_tabl_tr == "?"] <- NA
  nodes=c(which(colnames(bin_tabl_tr)%like%"Progne"),base)
  colnames(bin_tabl_tr)=gsub(base,"Basal Node",colnames(bin_tabl_tr))
  
  # get column order to understand
  # Phylogenetic placement - out to in, large to small
  col_order=rev(c("Basal Node",
              "Progne tapera",
              "Progne murphyi",
              "Progne subis",
              "Progne elegans",
              "Progne chalybea",
              "Progne sinaloae",
              "Progne dominicensis",
              "Progne cryptoleuca"))
  
  col_theme <- rasterTheme(region = c("#377eb8","#e41a1c"))
  plot2=levelplot(bin_tabl_tr[,col_order], scales = list(x = list(rot = 90)), # labels
          xlab = var.lab.2, ylab = "", 
          par.settings = col_theme, colorkey = FALSE) # color options
  
  print(plot2)
  print("Complete.")
}
```

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-5.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-6.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-7.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-8.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-9.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-10.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-11.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-12.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-13.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-14.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-15.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-16.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-17.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-18.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-19.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-20.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-21.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-22.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-23.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-24.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-25.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-26.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-27.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-28.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-29.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-30.png)<!-- -->

    ## [1] "Complete."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-31.png)<!-- -->

    ## [1] "Starting LevelPlot..."

![](Appendix1_v2_files/figure-gfm/unnamed-chunk-27-32.png)<!-- -->

    ## [1] "Complete."

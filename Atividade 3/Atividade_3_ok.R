###Disciplina de Ciencia Colaborativa###

#### ATIVIDADE 3 - ACESSO A BANCO DE DADOS ABERTOS ####

###CARREGAR PACOTES###
library(tidyverse)
##Instalando pacote rgbif##
library(rgbif)

##GBIF##
# checar funcoes
?occ_data
# baixar ocorrencias
cacha_gbif <- occ_data(scientificName = "Physeter macrocephalus", 
                       hasCoordinate = TRUE,
                       hasGeospatialIssue=FALSE)

# dimensoes

dim(cacha_gbif)

## NULL

dim(cacha_gbif$data)
## [1] 500 122

# checar campos
cacha_gbif$data %>% names

##Problemas reportados##

#Checar issus#

gbif_issues()

issues_gbif <- cacha_gbif$data$issues %>%
  # unique() %>%
  strsplit(.,"[,]") %>%
  unlist() %>%
  unique()

issues_gbif

[1] "osiic"    "cdround"  "inmano"   "gass84"   "cudc"     "fpwktinv" "colmano" 
[8] "inmafu"   "gdativ"

#48       osiic  OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT
#10     cdround                                COORDINATE_ROUNDED
#58      inmano                            INSTITUTION_MATCH_NONE
#23      gass84                      GEODETIC_DATUM_ASSUMED_WGS84
#57      inmafu                           INSTITUTION_MATCH_FUZZY
#12        cudc                  COUNTRY_DERIVED_FROM_COORDINATES
#55     colmano                             COLLECTION_MATCH_NONE
#24      gdativ                            GEODETIC_DATUM_INVALID
#63    fpwktinv                             FOOTPRINT_WKT_INVALID

library(dplyr)

# ver quais os problemas encontrados no dataset baixado
gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif)

# selecionar campos de interesse
cacha_gbif1 <- cacha_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude,issues,
                basisOfRecord, sex, rightsHolder, 
                datasetName, recordedBy, waterBody, country)

# ocorrencias únicas
cacha_gbif1 <- cacha_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(cacha_gbif1, unique)

$scientificName
[1] "Physeter macrocephalus Linnaeus, 1758" "Physeter catodon Linnaeus, 1758"      

$decimalLatitude
[1]  53.229740  57.038607 -42.513421  53.233444 -36.253753   3.172525   3.193708
[8]   3.182405   3.173467   3.146190   3.046789   3.105529   3.067261   3.099359
[15]  69.529612  21.193178  20.864816 -38.443199 -34.666047 -34.694203 -34.650155
[22] -34.668158 -38.530000 -42.583345  27.353921  40.355064   0.228051 -34.662017
[29] -34.659722 -34.627558  36.941588  37.035517  36.970796  36.906790 -34.156041
[36]  69.414462  72.055390 -42.687137  37.470764  37.540593  37.405967  37.404381
[43] -46.413400  36.875517  53.145000  38.388565  38.391527  38.396098  37.989720
[50]  37.814376  69.333318  43.766734  25.079536  38.096015  37.954962  64.810520
[57]  43.940000  37.590567  42.996061  52.667470  43.049778  41.156506  69.399703
[64]  37.694844  41.863016  38.654968  38.615930  38.689125 -37.503292  57.905753
[71]  36.076367  19.896102  18.975035  20.829486 -42.437301  57.567727 -42.523676
[78]  57.213250 -34.942833 -34.936833 -35.779167 -34.972333 -34.945000 -34.919167
[85] -34.952667 -34.777833 -34.768500 -35.686833  16.184533  32.664813 -42.629244
[92]  15.504014  44.690955  44.637447 -46.204985 -32.612520 -42.408060  16.362554
[99]  51.400000 -42.421450 -35.939167 -34.722833 -35.566000 -34.933000 -35.687833
[106] -35.052000 -35.011167 -34.109675  16.116983   9.327151  57.275383 -35.037000
[113] -35.029667 -35.064167 -34.976833 -34.864433  16.193783  38.469890  38.465535
[120]  38.655572  38.295733  38.682845  38.298692  38.311770  38.658972  38.666713
[127]  38.450298  38.452267  38.659420  38.650187  38.460340  38.481275  38.469795
[134]  38.464305  38.668870  38.681238  38.659080  38.658965  38.668967  37.008110
[141]  36.873803  36.892680  36.895950  36.905006  33.110396  38.427917  38.430842
[148]  38.437910  38.413185  38.419955  38.393562  38.400187  38.389818  38.389967
[155]  38.331405  38.323525  38.316163  38.332202  38.384852  38.411847  38.415327
[162]  38.406312  38.406463  38.403422  38.394020  38.403597  38.467735  38.455298
[169]  38.453993  38.457307  38.358425  38.358270  38.341277  38.500000  38.358817
[176]  38.346597  38.320200  38.324618  71.093846  69.431271  69.235499  67.271668
[183]  69.418419  69.428549  23.849956  23.911043 -33.973623  37.599850  38.267870
[190]  38.298306  38.267045  43.076650  38.428985  38.420693  38.382860  38.419545
[197]  38.419443  38.428918  38.423552  38.310010  38.386750  38.386988  38.420322
[204]  38.387677  38.317748  38.383578  38.397127  38.387548  38.417948  38.350077
[211]  38.350067  38.323800  38.395405  38.389888  38.335697  38.331198  38.331148
[218]  38.325102  38.336877  38.417283  38.418807  38.341495  38.335182  38.333830
[225]  38.330875  38.337672  38.350988  38.334582  38.673907  38.678587  38.282178
[232]  38.274897  38.265467  38.302417  38.675860  38.693340  38.739590  38.675585
[239]  38.742270  38.739620  38.739682  38.742757  38.744550  38.378695  38.386137
[246]  38.382808  38.400500  38.378628  38.391827  38.367448  38.378482  38.277340
[253]  38.400405  38.400962  38.434483  38.449988  38.322975  38.321110  38.369637
[260]  38.366880  38.398888  38.397017  38.293660  38.596285  38.393597  38.290532
[267]  38.301633  69.551544  39.127549  32.846397  37.622343  43.058980  38.622988
[274]  38.632372  38.634673  38.655962  38.623648  38.623035  38.389547  38.695487
[281]  38.704672  38.616762  38.620125  38.718540  38.717720  38.738373  38.626580
[288]  38.634773  38.693670  38.694472  38.718255  38.625393  38.746630  38.530997
[295]  38.522540  38.746747  38.701983  38.704592  38.533140  38.528652  38.702942
[302]  38.640032  38.653618  38.653942  38.667620  38.546365  38.546647  38.549932
[309]  38.547203  38.391270  38.573762  38.331617  38.336685  38.496843  38.494788
[316]  38.488212  38.708137  16.031236  38.643736  38.937083  42.204330  42.206950
[323]  42.159630  42.191380  38.643037  38.663812  38.701897  38.631787  38.629937
[330]  38.703622  38.700442  38.314485  38.308447  38.632078  38.632060  38.688253
[337]  38.643185  38.333755  38.363782  38.357332  38.654307  38.641783  38.624933
[344]  38.644538  38.630908  38.625915  38.609838  38.630503  38.749290  38.746292
[351]  38.609845  38.613198  38.613490  38.746305  38.731788  38.731587  38.728723
[358]  38.648470  38.641258  38.352278  38.225605  38.209838  34.055224  42.868171
[365]  38.330300  38.480598  38.330155  38.474830  38.338458  38.365427 -42.487611
[372] -42.523630  58.259117 -38.525653  24.125317  16.720539  53.000000  40.226540
[379]  28.049313 -42.554790  25.624173  58.545381  58.467503 -42.466905 -42.503490
[386]  69.408352 -42.553555  32.937320  32.934990  32.934870  33.505470  32.847153
[393]  32.790807  32.887857  32.668468  32.918618  32.884268  32.889088  32.919091
[400]  32.619105  32.673100  32.593476  33.896895  32.947831  32.931452  32.947223
[407]  34.035716  33.821861  33.808842  33.860968  32.939756  32.947479  32.923079
[414]  34.385104  34.263195  34.190281  34.209202  32.680626  32.696784  34.462970
[421]  34.440089  34.038503  34.037783  34.035906  32.649928  32.683806  32.032506
[428]  32.024085  32.034798  32.070968  32.081337  32.134752  32.037881  33.507607
[435]  33.518924  33.519215  33.555385  33.491640  33.495662  33.493654  33.484277
[442]  33.479308  33.493390  33.508647  33.516396  33.512522  33.497174  33.511918
[449]  32.621764  33.196218  33.421399  32.562871

$decimalLongitude
[1]    4.879078   -7.420526  173.742860    4.882180  175.277187    9.908757
[7]    9.833179    9.828299    9.879258    9.988282    9.802350    9.990787
[13]    9.842173    9.846989   16.852188  -97.480488  -97.246434  145.205813
[19]  119.525804  119.576912  119.598026  119.537601  145.320000  173.768222
[25]  -80.224777   23.975900  -91.805959  119.561892  119.603090  119.592533
[31]  -74.340162  -74.460772  -74.324153  -74.452957   18.424374   11.859954
[37]   17.716888  173.749407  -25.708126  -25.793444  -25.703438  -25.623137
[43]  168.239080  -74.118750  -16.695000  -28.351000  -28.360963  -28.365493
[49]  -25.846863  -25.986633   15.978244    7.694213 -110.613228  -25.880375
[55]  -25.903016  -23.078747  -67.450000  -25.626082    6.849040  -34.312028
[61]    8.805410  -29.878992   15.946552  -25.179017  -65.433838  -28.634176
[67]  -28.765994  -28.640381  149.792191   -3.820750 -122.114413  -43.866523
[73]  -51.052370 -156.872508  173.938713   -4.091400  173.692922   -7.422880
[79]   24.068167   24.055667   24.203333   24.018000   24.072167   24.371833
[85]   24.368833   24.849833   24.843000   23.020833  -61.872067 -117.326603
[91]  174.197067  -61.457263 -124.169565 -124.010745  166.639730  152.325740
[97]  173.627846  -86.817703    0.900000  173.871232   24.078833   25.009500
[103]   23.810833   24.179000   23.425667   24.206167   24.021333  174.823004
[109]  -61.922183  -81.631402   -7.422069   24.193167   24.227500   24.216000
[115]   24.446500   24.494533  -61.866050  -28.803117  -28.793668  -28.593550
[121]  -28.262113  -28.631727  -28.263683  -28.237215  -28.609067  -28.622952
[127]  -28.766737  -28.775950  -28.612048  -28.559992  -28.941885  -28.934670
[133]  -28.968782  -28.958175  -28.564645  -28.562517  -28.564297  -28.564312
[139]  -28.564830  -74.472908  -74.481985  -74.492095  -74.498162  -74.455269
[145]   35.180098  -28.620138  -28.621133  -28.626078  -28.603908  -28.603580
[151]  -28.433617  -28.446162  -28.319875  -28.301970  -28.331533  -28.349290
[157]  -28.393640  -28.431418  -28.328020  -28.602000  -28.549642  -28.572402
[163]  -28.575183  -28.537733  -28.535463  -28.547683  -28.747253  -28.715923
[169]  -28.715305  -28.738010  -28.351880  -28.353950  -28.349473  -28.600000
[175]  -28.350517  -28.353892  -28.357873  -28.386430   25.312719   15.515621
[181]   15.570625   14.709733   15.493045   15.457210  121.777345  121.976406
[187]   18.498546  -25.858169  -28.373137  -28.205123  -28.199630  145.171390
[193]  -28.763773  -28.766668  -28.481572  -28.732667  -28.735355  -28.761310
[199]  -28.651163  -28.484023  -28.467400  -28.468047  -28.766845  -28.484323
[205]  -28.391668  -28.523477  -28.506455  -28.457908  -28.777387  -28.436888
[211]  -28.436645  -28.427623  -28.464592  -28.450707  -28.420392  -28.400745
[217]  -28.400775  -28.385360  -28.414687  -28.652953  -28.654655  -28.464350
[223]  -28.469915  -28.464977  -28.374843  -28.439965  -28.437752  -28.462602
[229]  -28.607913  -28.612772  -28.409798  -28.401902  -28.405723  -28.413122
[235]  -28.621318  -28.643057  -28.826445  -28.620320  -28.834287  -28.826510
[241]  -28.826602  -28.833072  -28.829227  -28.447580  -28.432100  -28.417987
[247]  -28.423822  -28.447752  -28.436335  -28.508523  -28.504687  -28.438862
[253]  -28.460683  -28.493995  -28.603347  -28.612212  -28.289728  -28.289033
[259]  -28.455582  -28.383032  -28.415685  -28.424053  -28.412762  -28.335235
[265]  -28.349078  -28.392490  -28.408562   17.488367   -9.366577  -16.637553
[271]  -25.908488  144.819390  -28.466800  -28.496967  -28.495633  -28.477208
[277]  -28.461197  -28.466568  -28.504597  -28.779135  -28.786085  -28.437192
[283]  -28.424940  -28.561730  -28.572905  -28.560735  -28.442808  -28.495552
[289]  -28.534027  -28.533787  -28.545127  -28.384767  -28.742937  -28.254922
[295]  -28.228525  -28.742975  -28.511227  -28.470557  -28.267070  -28.261868
[301]  -28.514145  -28.529365  -28.564573  -28.564792  -28.580573  -28.266268
[307]  -28.267527  -28.279400  -28.298135  -28.455030  -28.272873  -28.418533
[313]  -28.353322  -28.806617  -28.774500  -28.791777  -28.447555  -86.856392
[319]  -28.694011   15.153582  -66.408920  -66.407470  -66.332670  -66.616150
[325]  -28.527232  -28.645862  -28.540272  -28.501470  -28.485180  -28.545685
[331]  -28.546805  -28.294333  -28.299573  -28.501498  -28.501495  -28.537515
[337]  -28.522858  -28.362542  -28.378547  -28.399118  -28.423082  -28.415980
[343]  -28.401963  -28.442518  -28.452763  -28.451902  -28.463937  -28.305760
[349]  -28.650477  -28.642762  -28.464015  -28.481582  -28.482153  -28.642635
[355]  -28.642600  -28.642438  -28.644527  -28.532288  -28.536810  -28.425085
[361]  -28.371300  -28.373283 -119.692730    6.059163  -28.004932  -28.711555
[367]  -28.511315  -28.734183  -28.510583  -28.524398  173.605088  173.483245
[373]   -6.135646  144.053814 -110.464368 -169.449418    1.100000  -69.300080
[379]  -14.329969  173.681197 -110.891803   -4.664942   -4.736217  173.690650
[385]  173.682175   16.152119  173.735440  -75.269740  -75.268330  -75.268670
[391]  -75.010650  -75.114134  -75.033926  -74.982762  -74.403937  -75.190026
[397]  -75.199077  -75.194126  -75.180154  -74.346355  -74.206993  -74.172567
[403]  -76.058935  -75.274339  -75.274429  -75.278411  -75.683787  -75.615379
[409]  -75.713868  -76.052778  -75.272516  -75.219854  -75.156453  -75.455129
[415]  -75.395533  -75.445156  -75.463304  -76.289284  -76.318124  -75.694063
[421]  -75.523381  -75.696229  -75.702330  -75.685915  -76.254583  -76.149112
[427]  -75.644629  -75.648342  -75.786200  -76.114977  -76.156403  -76.243959
[433]  -76.306056  -75.114648  -75.065674  -75.119990  -75.031828  -74.984248
[439]  -75.006924  -74.975103  -74.972245  -74.980728  -74.980661  -75.002465
[445]  -75.002784  -75.005538  -75.103605  -74.999072  -73.881430  -74.561181
[451]  -74.913299  -74.053628

$issues
[1] "osiic"                       "cdround"                    
[3] "osiic,inmano"                "cdround,gass84"             
[5] "cudc,fpwktinv"               "colmano,inmafu"             
[7] "cdround,cudc"                "fpwktinv"                   
[9] "cdround,osiic"               "cdround,gass84,gdativ"      
[11] "gass84,gdativ"               ""                           
[13] "cudc,gass84,gdativ,fpwktinv"

$basisOfRecord
[1] "HUMAN_OBSERVATION"   "PRESERVED_SPECIMEN"  "MATERIAL_SAMPLE"    
[4] "MACHINE_OBSERVATION"

$sex
[1] "MALE"   NA       "FEMALE"

$rightsHolder
[1] NA                              "Carita Bergman"               
[3] "Joe Potter Butler"             "Naturalis Biodiversity Center"
[5] "Daniel Percy"                  "Siren Sighting Network_AMMCO" 
[7] "Roberto Ansoleaga"             "kaotay"                       
[9] "pia markovic"                  "Amaya M."                     
[11] "sfitzgerald86"                 "Νίκος Διακάτος"               
[13] "taopina"                       "Amy Engelhaupt"               
[15] "Jeltje van den Bosch"          "corokid"                      
[17] "David Rosario"                 "Lloyd Esler"                  
[19] "Kirstin Jones"                 "Lisa Steiner"                 
[21] "rjtizard"                      "Elan Zucchetti"               
[23] "sandor_in"                     "aristotelis"                  
[25] "Raphael von Büren"             "Beverly Orthwein"             
[27] "Mouser Williams"               "De France Arthur"             
[29] "Lawrence Haselmaier"           "Julien Renoult"               
[31] "Daniel Santos Marques"         "Jeannine Winkel"              
[33] "Pedro Beja"                    "martinbutterfield"            
[35] "alessandra"                    "John Garrett"                 
[37] "Charlotte Kirchner"            "Bob Thomson"                  
[39] "captvucsa"                     "Stichting Natuurinformatie"   
[41] "OMMAG"                         "Erich Starzinger"             
[43] "Madelon Stuut"                 "Tim Smoot"                    
[45] "P Holroyd"                     "Ashley Dineen"                
[47] "Danilo Hegg"                   "Lily"                         
[49] "dpellman"                      "Niels Peter Møller Jensen"    
[51] "mefsnz"                        "Evan Centanni"                
[53] "Ben Shemer"                    "Geir Drange"                  
[55] "dianavx"                       "楊淇筌"                       
[57] "Dewald du Plessis"             "Sabine+Ulf"                   
[59] "arrojamentos-portugal"         "Fábio Olmos"                  
[61] "Beatriz"                       "Yvonne Scherrer"              
[63] "dharris73"                     "claudereveret"                
[65] "Kyle Bland"                    "Dougal Townsend"              
[67] "possumpete"                    "Ricardo Rivera"               
[69] "Ryan Rash"                     "seung hee, han"               
[71] "Ivan Phillipsen"               "christiankropf"               

$datasetName
[1] NA                                                                                                     
[2] "iNaturalist research-grade observations"                                                              
[3] "Happywhale - Sperm Whale in Indian Ocean"                                                             
[4] "Happywhale - Sperm Whale in North Atlantic Ocean"                                                     
[5] "Happywhale - Sperm Whale in North Pacific Ocean"                                                      
[6] "Happywhale - Sperm Whale in South Pacific Ocean"                                                      
[7] "KYMA boat-based surveys in Thyrrhenian Sea and Ionian Sea"                                            
[8] "NEFSC Right Whale Aerial Survey"                                                                      
[9] "Sightings from R/V Song of the Whale during the winter 2019 MAPS survey (NMFS permit 14809)"          
[10] "Acoustic detections from R/V Song of the Whale during the winter 2019 MAPS survey (NMFS permit 14809)"

$recordedBy
[1] "Kjeld Venema, Annemarie van den Berg / SOS Dolfijn"                                                        
[2] "Carita Bergman"                                                                                            
[3] "Joe Potter Butler"                                                                                         
[4] "Snijteam Naturalis"                                                                                        
[5] "Daniel Percy"                                                                                              
[6] "Siren Sighting Network_AMMCO"                                                                              
[7] "Timo Meißner"                                                                                              
[8] "Roberto Ansoleaga"                                                                                         
[9] "kaotay"                                                                                                    
[10] "pia markovic"                                                                                              
[11] "Department of Environment, Land, Water and Planning (DELWP)"                                               
[12] "Amaya M."                                                                                                  
[13] "sfitzgerald86"                                                                                             
[14] "Νίκος Διακάτος"                                                                                            
[15] "taopina"                                                                                                   
[16] "Amy Engelhaupt"                                                                                            
[17] "Jeltje van den Bosch"                                                                                      
[18] "corokid"                                                                                                   
[19] "David Rosario"                                                                                             
[20] "Lloyd Esler"                                                                                               
[21] "Kirstin Jones"                                                                                             
[22] NA                                                                                                          
[23] "rjtizard"                                                                                                  
[24] "Per Ivar Nicolaisen"                                                                                       
[25] "Elan Zucchetti"                                                                                            
[26] "sandor_in"                                                                                                 
[27] "aristotelis"                                                                                               
[28] "Raphael von Büren"                                                                                         
[29] "Beverly Orthwein"                                                                                          
[30] "Mouser Williams"                                                                                           
[31] "De France Arthur"                                                                                          
[32] "Lawrence Haselmaier"                                                                                       
[33] "Julien Renoult"                                                                                            
[34] "Jonas Langbråten|Bjørn Olav Tveit|Trond Ove Stakkeland|Ole Skimmeland|Tor Olsen|Torborg Berge|Steve Baines"
[35] "Daniel Santos Marques"                                                                                     
[36] "Jeannine Winkel"                                                                                           
[37] "Pedro Beja"                                                                                                
[38] "martinbutterfield"                                                                                         
[39] "alessandra"                                                                                                
[40] "John Garrett"                                                                                              
[41] "Charlotte Kirchner"                                                                                        
[42] "Bob Thomson"                                                                                               
[43] "captvucsa"                                                                                                 
[44] "Smass"                                                                                                     
[45] "Brooks, C. Chris"                                                                                          
[46] "OMMAG"                                                                                                     
[47] "Erich Starzinger"                                                                                          
[48] "Madelon Stuut"                                                                                             
[49] "Tim Smoot"                                                                                                 
[50] "P Holroyd"                                                                                                 
[51] "Ashley Dineen"                                                                                             
[52] "Danilo Hegg"                                                                                               
[53] "SCHM-035"                                                                                                  
[54] "Lily"                                                                                                      
[55] "dpellman"                                                                                                  
[56] "Partridge, F. Francesca"                                                                                   
[57] "Niels Peter Møller Jensen"                                                                                 
[58] "mefsnz"                                                                                                    
[59] "Evan Centanni"                                                                                             
[60] "Scottish Marine Animal Stranding Scheme"                                                                   
[61] "Lisa Steiner"                                                                                              
[62] "Ben Shemer"                                                                                                
[63] "Karel Samyn"                                                                                               
[64] "Geir Drange"                                                                                               
[65] "Stig  Rasmussen"                                                                                           
[66] "Simon Rix"                                                                                                 
[67] "dianavx"                                                                                                   
[68] "楊淇筌"                                                                                                    
[69] "Dewald du Plessis"                                                                                         
[70] "arrojamentos-portugal"                                                                                     
[71] "Fábio Olmos"                                                                                               
[72] "Beatriz"                                                                                                   
[73] "Yvonne Scherrer"                                                                                           
[74] "dharris73"                                                                                                 
[75] "claudereveret"                                                                                             
[76] "Kyle Bland"                                                                                                
[77] "Dougal Townsend"                                                                                           
[78] "Dodd, S. Steve"                                                                                            
[79] "possumpete"                                                                                                
[80] "Ricardo Rivera"                                                                                            
[81] "Ryan Rash"                                                                                                 
[82] "Lewis, S. Ste"                                                                                             
[83] "1833317366"                                                                                                
[84] "seung hee, han"                                                                                            
[85] "Ivan Phillipsen"                                                                                           
[86] "British Divers Marine Life Rescue"                                                                         
[87] "Haines, D. David"                                                                                          
[88] "christiankropf"                                                                                            

$waterBody
[1] NA                                            
[2] "Great Australian Bight"                      
[3] "North Atlantic Ocean"                        
[4] "Caribbean Sea"                               
[5] "North Pacific Ocean"                         
[6] "South Pacific Ocean"                         
[7] "Pacific Ocean"                               
[8] "Mediterranean Sea,Thyrrhenian Sea,Ionian Sea"
[9] "North Atlantic"                              

$country
[1] "Netherlands"                                         
[2] "United Kingdom of Great Britain and Northern Ireland"
[3] "New Zealand"                                         
[4] "Cameroon"                                            
[5] "Norway"                                              
[6] "Mexico"                                              
[7] "Australia"                                           
[8] "United States of America"                            
[9] "Greece"                                              
[10] "Ecuador"                                             
[11] "South Africa"                                        
[12] "Portugal"                                            
[13] NA                                                    
[14] "Italy"                                               
[15] "Iceland"                                             
[16] "France"                                              
[17] "Canada"                                              
[18] "Guadeloupe"                                          
[19] "Dominica"                                            
[20] "Honduras"                                            
[21] "Panama"                                              
[22] "Israel"                                              
[23] "Chinese Taipei"                                      
[24] "Japan"                                               
[25] "United States Minor Outlying Islands"                
[26] "Spain" 


## Problemas não reportados

# investigar niveis suspeitos
cacha_gbif1 %>% 
  distinct(country) %>% 
  pull()

# country
cacha_gbif1 %>%
  group_by(country) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=country)) +
  geom_bar(stat = 'identity') 

# investigar niveis suspeitos
cacha_gbif1 %>% 
  distinct(scientificName) %>% 
  pull()

# scientificName
cacha_gbif1 %>%
  group_by(scientificName) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=scientificName)) +
  geom_bar(stat = 'identity')

# investigar niveis suspeitos
cacha_gbif1 %>% 
  distinct(basisOfRecord) %>% 
  pull()

[1] "HUMAN_OBSERVATION"   "PRESERVED_SPECIMEN"  "MATERIAL_SAMPLE"    
[4] "MACHINE_OBSERVATION"

# basisOfRecord
cacha_gbif1 %>%
  group_by(basisOfRecord) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y= basisOfRecord)) +
  geom_bar(stat = 'identity')

## A informação da "PRESERVED_SPECIMEN" apresentou outra especie de cachalote (Physeter catodon Linnaeus, 1758). Irei remover ela das analises

# fonte das regioes erradas
cacha_gbif1 %>% 
  filter(basisOfRecord %in% c("PRESERVED_SPECIMEN")) %>% 
  distinct(datasetName)

# filtrar todas do dataset suspeito
cacha_gbif_ok <- cacha_gbif1 %>% 
  filter(!datasetName %in% c("PRESERVED_SPECIMEN"))


## PLotar ocorrencias

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = cacha_gbif_ok,aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Physeter macrocephalus")))



### extrair dados do OBIS
library(robis)

# baixar ocorrências
cacha_obis <- robis::occurrence("Physeter macrocephalus")
#Retrieved 78837 records of approximately 78837 (100%)

# checar dados
names(cacha_obis)

[1] "date_year"                     "scientificNameID"             
[3] "year"                          "scientificName"               
[5] "superfamilyid"                 "dropped"                      
[7] "aphiaID"                       "decimalLatitude"              
[9] "subclassid"                    "phylumid"                     
[11] "familyid"                      "catalogNumber"                
[13] "occurrenceStatus"              "basisOfRecord"                
[15] "superclass"                    "modified"                     
[17] "id"                            "order"                        
[19] "superclassid"                  "infraorderid"                 
[21] "dataset_id"                    "decimalLongitude"             
[23] "collectionCode"                "date_end"                     
[25] "speciesid"                     "occurrenceID"                 
[27] "superfamily"                   "suborderid"                   
[29] "date_start"                    "genus"                        
[31] "bibliographicCitation"         "eventDate"                    
[33] "absence"                       "genusid"                      
[35] "originalScientificName"        "marine"                       
[37] "subphylumid"                   "institutionCode"              
[39] "date_mid"                      "class"                        
[41] "suborder"                      "infraorder"                   
[43] "orderid"                       "geodeticDatum"                
[45] "kingdom"                       "classid"                      
[47] "phylum"                        "species"                      
[49] "subphylum"                     "subclass"                     
[51] "datasetID"                     "family"                       
[53] "category"                      "kingdomid"                    
[55] "node_id"                       "flags"                        
[57] "sss"                           "shoredistance"                
[59] "sst"                           "bathymetry"                   
[61] "day"                           "month"                        
[63] "eventID"                       "scientificNameAuthorship"     
[65] "individualCount"               "associatedReferences"         
[67] "type"                          "taxonRemarks"                 
[69] "recordNumber"                  "georeferencedDate"            
[71] "verbatimEventDate"             "license"                      
[73] "dateIdentified"                "ownerInstitutionCode"         
[75] "coordinateUncertaintyInMeters" "taxonRank"                    
[77] "vernacularName"                "identificationRemarks"        
[79] "nomenclaturalCode"             "footprintWKT"                 
[81] "datasetName"                   "taxonomicStatus"              
[83] "waterBody"                     "specificEpithet"              
[85] "coordinatePrecision"           "occurrenceRemarks"            
[87] "country"                       "maximumDepthInMeters"         
[89] "organismQuantity"              "organismQuantityType"         
[91] "minimumDepthInMeters"          "sex"                          
[93] "lifeStage"                     "depth"                        
[95] "locality"                      "eventTime"                    
[97] "rightsHolder"                  "institutionID"                
[99] "language"                      "sampleSizeUnit"               
[101] "sampleSizeValue"               "fieldNumber"                  
[103] "stateProvince"                 "continent"                    
[105] "recordedBy"                    "organismID"                   
[107] "organismRemarks"               "informationWithheld"          
[109] "identifiedBy"                  "associatedMedia"              
[111] "taxonConceptID"                "references"                   
[113] "higherGeography"               "samplingProtocol"             
[115] "identificationReferences"      "fieldNotes"                   
[117] "behavior"                      "habitat"                      
[119] "parentEventID"                 "footprintSRS"                 
[121] "samplingEffort"                "eventRemarks"                 
[123] "georeferenceRemarks"           "dynamicProperties"            
[125] "georeferencedBy"               "minimumElevationInMeters"     
[127] "maximumElevationInMeters"      "georeferenceProtocol"         
[129] "verbatimDepth"                 "typeStatus"                   
[131] "countryCode"                   "startDayOfYear"               
[133] "otherCatalogNumbers"           "collectionID"                 
[135] "verbatimLocality"              "identificationID"             
[137] "accessRights"                  "locationRemarks"              
[139] "islandGroup"                   "locationID"                   
[141] "verbatimLatitude"              "rights"                       
[143] "verbatimLongitude"             "verbatimCoordinateSystem"     
[145] "preparations"                  "endDayOfYear"                 
[147] "municipality"                  "island"                       
[149] "taxonID"                       "county"                       
[151] "nameAccordingToID"             "acceptedNameUsage"            
[153] "nameAccordingTo"               "dataGeneralizations"          
[155] "georeferenceSources"  


cacha_obis1 <- cacha_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder,
                datasetName, recordedBy, depth, locality, habitat) %>%
  distinct()

# check problemas reportados (flags)
cacha_obis1 %>% 
  distinct(flags)

# A tibble: 8 x 1
flags                     
<chr>                     
  1 no_depth                  
2 NA                        
3 no_depth,on_land          
4 depth_exceeds_bath,on_land
5 on_land,no_depth          
6 on_land                   
7 depth_exceeds_bath        
8 on_land,depth_exceeds_bath


# check NA em datasetName
cacha_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)
# A tibble: 10 x 1
waterBody                       
<chr>                           
  1 NA                              
2 Southern Ocean                  
3 Arctic                          
4 South Pacific Ocean             
5 Atlantic                        
6 Gulf of Mexico                  
7 South Pacific Ocean; Open waters
8 Atlantic Ocean                  
9 Isla de Malpelo                 
10 Pacifc Ocean

# depth ok
cacha_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),%>%
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


# checar niveis
cacha_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("world, NA, Global, g")) %>% 
  lapply(., unique)

# 
cacha_obis_ok <- cacha_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("world, NA, Global, g")) 

# plotar mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = cacha_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Physeter macrocephalus")))

##NÃO CONSEGUI PLOTAR O MAPA. PRECISO REMOVER ALGUMAS VARIAVEIS QUE APRESENTARAM ERRO

# checar pontos sem a legenda de localidades
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = cacha_obis_ok,aes(x = decimalLongitude, y = decimalLatitude), color = "blue") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Physeter macrocephalus")))
################################################
# fonte das regioes erradas
cacha_obis1 %>% 
  filter(waterBody %in% c("world, NA, Global, g")) %>% 
  distinct(datasetName)

# filtrar todas do dataset suspeito
cacha_obis_ok <- cacha_obis1 %>% 
  filter(!datasetName %in% c("world, NA, Global, g"))

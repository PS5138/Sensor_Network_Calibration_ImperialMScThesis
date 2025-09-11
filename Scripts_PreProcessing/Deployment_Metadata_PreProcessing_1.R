# Data Preprocessing for deployment sites metadata
# Created by Parivrudh Sharma 

###########################
rm(list=ls())
library(dplyr)
library(sf)  
library(ggplot2)
###########################

setwd("/Users/parivrudhsharma/Desktop/Desktop/Imperial College London/MSc Health Data Analytics and Machine Learning/Modules/Research Project/Data")

deployment_md <- read.csv("Data/DeploymentSitesMetadata.csv")

View(deployment_md)

###########################

# deployment_md needs to be cleaned up - random rows seem to be shifted 
# remove last 2 rows first - 

deployment_md <- deployment_md[1:(nrow(deployment_md) - 2), ]
# 715 rows now

# Fix the malformed row manually (overwrite it)
deployment_md[47, "SiteName"] <- "Tavistock Road, Hillingdon"
deployment_md[47, "Latitude"] <- "51.51138"
deployment_md[47, "Longitude"] <- "-0.477711"
deployment_md[47, "SiteClassification"] <- "Roadside"
deployment_md[47, "HeadHeight"] <- "2.5"
deployment_md[47, "ToRoad"] <- "0.75"

deployment_md <- deployment_md[-48, ]
rownames(deployment_md) <- NULL

deployment_md[86, "SiteName"] <- "Goresbrook School, Dagenham"
deployment_md[86, "Latitude"] <- "51.53167"
deployment_md[86, "Longitude"] <- "0.1305556"
deployment_md[86, "SiteClassification"] <- "Urban Background"
deployment_md[86, "HeadHeight"] <- "2.5"
deployment_md[86, "ToRoad"] <- "17"

deployment_md <- deployment_md[-87, ]
rownames(deployment_md) <- NULL

deployment_md[87, "SiteName"] <- "Al Maanar, Acklam Road -  Kensington"
deployment_md[87, "Latitude"] <- "51.52146"
deployment_md[87, "Longitude"] <- "-0.2037757"
deployment_md[87, "SiteClassification"] <- "Roadside"
deployment_md[87, "HeadHeight"] <- "3.1"
deployment_md[87, "ToRoad"] <- "1.7"

deployment_md <- deployment_md[-88, ]
rownames(deployment_md) <- NULL

deployment_md[111, "SiteName"] <- "George Street, Richmond"
deployment_md[111, "Latitude"] <- "51.46106"
deployment_md[111, "Longitude"] <- "-0.3040147"
deployment_md[111, "SiteClassification"] <- "Roadside"
deployment_md[111, "HeadHeight"] <- "2.6"
deployment_md[111, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-112, ]
rownames(deployment_md) <- NULL

deployment_md[114, "SiteName"] <- "Wembley High Road, Cafe Quarter"
deployment_md[114, "Latitude"] <- "51.55362"
deployment_md[114, "Longitude"] <- "-0.290982"
deployment_md[114, "SiteClassification"] <- "Roadside"
deployment_md[114, "HeadHeight"] <- "2.4"
deployment_md[114, "ToRoad"] <- "0.37"

deployment_md <- deployment_md[-115, ]
rownames(deployment_md) <- NULL

deployment_md[115, "SiteName"] <- "Carlton Vale Road, Kilburn Park School"
deployment_md[115, "Latitude"] <- "51.5326"
deployment_md[115, "Longitude"] <- "-0.199633"
deployment_md[115, "SiteClassification"] <- "Roadside"
deployment_md[115, "HeadHeight"] <- "2.4"
deployment_md[115, "ToRoad"] <- "2.8"

deployment_md <- deployment_md[-116, ]
rownames(deployment_md) <- NULL

deployment_md[123, "SiteName"] <- "Kew Rd, Richmond, nr taxi rank/opposite Richmond station"
deployment_md[123, "Latitude"] <- "51.464"
deployment_md[123, "Longitude"] <- "-0.3015957"
deployment_md[123, "SiteClassification"] <- "Roadside"
deployment_md[123, "HeadHeight"] <- "2.6"
deployment_md[123, "ToRoad"] <- "3.3"

deployment_md <- deployment_md[-124, ]
rownames(deployment_md) <- NULL

deployment_md[125, "SiteName"] <- "High St, Hampton Hill, opposite j/w Windmill Rd"
deployment_md[125, "Latitude"] <- "51.4257"
deployment_md[125, "Longitude"] <- "-0.3569041"
deployment_md[125, "SiteClassification"] <- "Roadside"
deployment_md[125, "HeadHeight"] <- "2.3"
deployment_md[125, "ToRoad"] <- "1.7"

deployment_md <- deployment_md[-126, ]
rownames(deployment_md) <- NULL

deployment_md[126, "SiteName"] <- "Park Rd,  Hampton Hill, nr j/w High St, TW11"
deployment_md[126, "Latitude"] <- "51.42923"
deployment_md[126, "Longitude"] <- "-0.3547406"
deployment_md[126, "SiteClassification"] <- "Roadside"
deployment_md[126, "HeadHeight"] <- "2.2"
deployment_md[126, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-127, ]
rownames(deployment_md) <- NULL

deployment_md[127, "SiteName"] <- "South Rd,  outside Lidl,  nr j/w Wellington/Hampton Rd TW2"
deployment_md[127, "Latitude"] <- "51.43558"
deployment_md[127, "Longitude"] <- "-0.3488877"
deployment_md[127, "SiteClassification"] <- "Roadside"
deployment_md[127, "HeadHeight"] <- "2.5"
deployment_md[127, "ToRoad"] <- "1.1"

deployment_md <- deployment_md[-128, ]
rownames(deployment_md) <- NULL

deployment_md[128, "SiteName"] <- "RFU Stadium, Whitton Rd j/w A316"
deployment_md[128, "Latitude"] <- "51.45442"
deployment_md[128, "Longitude"] <- "-0.3381662"
deployment_md[128, "SiteClassification"] <- "Roadside"
deployment_md[128, "HeadHeight"] <- "2.2"
deployment_md[128, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-129, ]
rownames(deployment_md) <- NULL

deployment_md[129, "SiteName"] <- "Burtons Road, Hampton Hill, west of j/w Albert Road, on railway bridge"
deployment_md[129, "Latitude"] <- "51.43083"
deployment_md[129, "Longitude"] <- "-0.3567385"
deployment_md[129, "SiteClassification"] <- "Roadside"
deployment_md[129, "HeadHeight"] <- "2.2"
deployment_md[129, "ToRoad"] <- "1"

deployment_md <- deployment_md[-130, ]
rownames(deployment_md) <- NULL

deployment_md[130, "SiteName"] <- "St John the Baptiste Primary Sch,  Lwr Teddington Rd KT1 4HQ ( between Seymour Rd and Broom Pk)"
deployment_md[130, "Latitude"] <- "51.41748"
deployment_md[130, "Longitude"] <- "-0.3096785"
deployment_md[130, "SiteClassification"] <- "Roadside"
deployment_md[130, "HeadHeight"] <- "2.4"
deployment_md[130, "ToRoad"] <- "0.3"

deployment_md <- deployment_md[-131, ]
rownames(deployment_md) <- NULL

deployment_md[131, "SiteName"] <- "Stanley Juniors Sch,  Strathmore Rd, Teddington TW11 8UH"
deployment_md[131, "Latitude"] <- "51.43386"
deployment_md[131, "Longitude"] <- "-0.3449203"
deployment_md[131, "SiteClassification"] <- "Roadside"
deployment_md[131, "HeadHeight"] <- "2.3"
deployment_md[131, "ToRoad"] <- "4"

deployment_md <- deployment_md[-132, ]
rownames(deployment_md) <- NULL

deployment_md[132, "SiteName"] <- "St Richard Reynolds College,  Clifden Rd,  north of j/w Copthall Gdns TW1"
deployment_md[132, "Latitude"] <- "51.4477"
deployment_md[132, "Longitude"] <- "-0.3337107"
deployment_md[132, "SiteClassification"] <- "Roadside"
deployment_md[132, "HeadHeight"] <- "2.4"
deployment_md[132, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-133, ]
rownames(deployment_md) <- NULL

deployment_md[133, "SiteName"] <- "Broad St, Teddington, nr Tescos + j/w Queens Rd TW11"
deployment_md[133, "Latitude"] <- "51.42603"
deployment_md[133, "Longitude"] <- "-0.3390728"
deployment_md[133, "SiteClassification"] <- "Roadside"
deployment_md[133, "HeadHeight"] <- "2.3"
deployment_md[133, "ToRoad"] <- "0.4"

deployment_md <- deployment_md[-134, ]
rownames(deployment_md) <- NULL

deployment_md[134, "SiteName"] <- "Hospital Brg Rd, north of Montrose Ave, Turing House School development"
deployment_md[134, "Latitude"] <- "51.45015"
deployment_md[134, "Longitude"] <- "-0.3669282"
deployment_md[134, "SiteClassification"] <- "Roadside"
deployment_md[134, "HeadHeight"] <- "2.5"
deployment_md[134, "ToRoad"] <- "0.8"

deployment_md <- deployment_md[-135, ]
rownames(deployment_md) <- NULL

deployment_md[135, "SiteName"] <- "Deer Park Schl/Lidl,  Richmond Rd, opp j/w Morley Rd, E Twickenham"
deployment_md[135, "Latitude"] <- "51.45555"
deployment_md[135, "Longitude"] <- "-0.3104137"
deployment_md[135, "SiteClassification"] <- "Roadside"
deployment_md[135, "HeadHeight"] <- "2.3"
deployment_md[135, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-136, ]
rownames(deployment_md) <- NULL

deployment_md[136, "SiteName"] <- "High St Teddington, nr Post Office and M&S"
deployment_md[136, "Latitude"] <- "51.42731"
deployment_md[136, "Longitude"] <- "-0.3297578"
deployment_md[136, "SiteClassification"] <- "Roadside"
deployment_md[136, "HeadHeight"] <- "2.3"
deployment_md[136, "ToRoad"] <- "1.6"

deployment_md <- deployment_md[-137, ]
rownames(deployment_md) <- NULL

deployment_md[137, "SiteName"] <- "St Margarets Rd,  opposite St Margarets station, TW1"
deployment_md[137, "Latitude"] <- "51.45527"
deployment_md[137, "Longitude"] <- "-0.3198755"
deployment_md[137, "SiteClassification"] <- "Roadside"
deployment_md[137, "HeadHeight"] <- "2.2"
deployment_md[137, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-138, ]
rownames(deployment_md) <- NULL

deployment_md[138, "SiteName"] <- "Kew Rd,  nr entrance to Kew Gardens,  (opp j/w The Avenue)"
deployment_md[138, "Latitude"] <- "51.47416"
deployment_md[138, "Longitude"] <- "-0.2923605"
deployment_md[138, "SiteClassification"] <- "Roadside"
deployment_md[138, "HeadHeight"] <- "2.3"
deployment_md[138, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-139, ]
rownames(deployment_md) <- NULL

deployment_md[140, "SiteName"] <- "Ceres Road,  j/w Bannockburn Road - Bannockburn Primary School"
deployment_md[140, "Latitude"] <- "51.48773"
deployment_md[140, "Longitude"] <- "0.09436271"
deployment_md[140, "SiteClassification"] <- "Roadside"
deployment_md[140, "HeadHeight"] <- "2.75"
deployment_md[140, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-141, ]
rownames(deployment_md) <- NULL

deployment_md[141, "SiteName"] <- "Greenwich University -Student Union,  Avery Hill Campus"
deployment_md[141, "Latitude"] <- "51.44629"
deployment_md[141, "Longitude"] <- "0.0780883"
deployment_md[141, "SiteClassification"] <- "Roadside"
deployment_md[141, "HeadHeight"] <- "2.7"
deployment_md[141, "ToRoad"] <- "100"

deployment_md <- deployment_md[-142, ]
rownames(deployment_md) <- NULL

deployment_md[142, "SiteName"] <- "Beresford Square, Woolwich,  London SE18 6AY"
deployment_md[142, "Latitude"] <- "51.49121"
deployment_md[142, "Longitude"] <- "0.06915209"
deployment_md[142, "SiteClassification"] <- "Urban Background"
deployment_md[142, "HeadHeight"] <- "3"
deployment_md[142, "ToRoad"] <- "39.76"

deployment_md <- deployment_md[-143, ]
rownames(deployment_md) <- NULL

deployment_md[143, "SiteName"] <- "Royal Hill, London,  SE10 8RZ,  adjacent to James Wolfe Primary School"
deployment_md[143, "Latitude"] <- "51.47664"
deployment_md[143, "Longitude"] <- "-0.009395774"
deployment_md[143, "SiteClassification"] <- "Roadside"
deployment_md[143, "HeadHeight"] <- "2.9"
deployment_md[143, "ToRoad"] <- "1.21"

deployment_md <- deployment_md[-144, ]
rownames(deployment_md) <- NULL

deployment_md[144, "SiteName"] <- "John Wilson Street, Woolwich,  adjacent to St Mary Magdalene C of E School"
deployment_md[144, "Latitude"] <- "51.49107"
deployment_md[144, "Longitude"] <- "0.06180736"
deployment_md[144, "SiteClassification"] <- "Roadside"
deployment_md[144, "HeadHeight"] <- "3"
deployment_md[144, "ToRoad"] <- "2.53"

deployment_md <- deployment_md[-145, ]
rownames(deployment_md) <- NULL

deployment_md[145, "SiteName"] <- "Amhurst Road, Hackney"
deployment_md[145, "Latitude"] <- "51.55273"
deployment_md[145, "Longitude"] <- "-0.0665931"
deployment_md[145, "SiteClassification"] <- "Roadside"
deployment_md[145, "HeadHeight"] <- "2.75"
deployment_md[145, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-146, ]
rownames(deployment_md) <- NULL

deployment_md[163, "SiteName"] <- "St Margaret’s Road, Twickenham"
deployment_md[163, "Latitude"] <- "51.46046"
deployment_md[163, "Longitude"] <- "-0.3226241"
deployment_md[163, "SiteClassification"] <- "Roadside"
deployment_md[163, "HeadHeight"] <- "2.7"
deployment_md[163, "ToRoad"] <- "1.2"

deployment_md <- deployment_md[-164, ]
rownames(deployment_md) <- NULL

deployment_md[164, "SiteName"] <- "Richmond upon Thames Schl + Richmond College,  Egerton Rd, Twickenham"
deployment_md[164, "Latitude"] <- "51.45255"
deployment_md[164, "Longitude"] <- "-0.3403844"
deployment_md[164, "SiteClassification"] <- "Roadside"
deployment_md[164, "HeadHeight"] <- "2.45"
deployment_md[164, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-165, ]
rownames(deployment_md) <- NULL

deployment_md[165, "SiteName"] <- "Strawberry Vale, Teddington"
deployment_md[165, "Latitude"] <- "51.43397"
deployment_md[165, "Longitude"] <- "-0.3295101"
deployment_md[165, "SiteClassification"] <- "Roadside"
deployment_md[165, "HeadHeight"] <- "2.5"
deployment_md[165, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-166, ]
rownames(deployment_md) <- NULL

deployment_md[166, "SiteName"] <- "Winchester Rd,   St Stephens Primary School"
deployment_md[166, "Latitude"] <- "51.45667"
deployment_md[166, "Longitude"] <- "-0.3229721"
deployment_md[166, "SiteClassification"] <- "Roadside"
deployment_md[166, "HeadHeight"] <- "2.4"
deployment_md[166, "ToRoad"] <- "0.2"

deployment_md <- deployment_md[-167, ]
rownames(deployment_md) <- NULL

deployment_md[167, "SiteName"] <- "King St,  Twickenham"
deployment_md[167, "Latitude"] <- "51.44638"
deployment_md[167, "Longitude"] <- "-0.3284767"
deployment_md[167, "SiteClassification"] <- "Roadside"
deployment_md[167, "HeadHeight"] <- "2.5"
deployment_md[167, "ToRoad"] <- "0.9"

deployment_md <- deployment_md[-168, ]
rownames(deployment_md) <- NULL

deployment_md[168, "SiteName"] <- "Paradise Rd, Richmond"
deployment_md[168, "Latitude"] <- "51.45978"
deployment_md[168, "Longitude"] <- "-0.3039311"
deployment_md[168, "SiteClassification"] <- "Roadside"
deployment_md[168, "HeadHeight"] <- "2.5"
deployment_md[168, "ToRoad"] <- "1.2"

deployment_md <- deployment_md[-169, ]
rownames(deployment_md) <- NULL

deployment_md[169, "SiteName"] <- "Manor Rd, west bound"
deployment_md[169, "Latitude"] <- "51.4293"
deployment_md[169, "Longitude"] <- "-0.3244353"
deployment_md[169, "SiteClassification"] <- "Roadside"
deployment_md[169, "HeadHeight"] <- "2.4"
deployment_md[169, "ToRoad"] <- "0.4"

deployment_md <- deployment_md[-170, ]
rownames(deployment_md) <- NULL

deployment_md[171, "SiteName"] <- "Twickenham railway bridge, London Rd, Twickenham"
deployment_md[171, "Latitude"] <- "51.44949"
deployment_md[171, "Longitude"] <- "-0.3300532"
deployment_md[171, "SiteClassification"] <- "Roadside"
deployment_md[171, "HeadHeight"] <- "2.3"
deployment_md[171, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-172, ]
rownames(deployment_md) <- NULL

deployment_md[172, "SiteName"] <- "High St, Whitton"
deployment_md[172, "Latitude"] <- "51.451"
deployment_md[172, "Longitude"] <- "-0.357864"
deployment_md[172, "SiteClassification"] <- "Roadside"
deployment_md[172, "HeadHeight"] <- "2.5"
deployment_md[172, "ToRoad"] <- "2.2"

deployment_md <- deployment_md[-173, ]
rownames(deployment_md) <- NULL

deployment_md[173, "SiteName"] <- "Staines Rd,  Twickenham jct"
deployment_md[173, "Latitude"] <- "51.44204"
deployment_md[173, "Longitude"] <- "-0.3525677"
deployment_md[173, "SiteClassification"] <- "Roadside"
deployment_md[173, "HeadHeight"] <- "2.6"
deployment_md[173, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-174, ]
rownames(deployment_md) <- NULL

deployment_md[175, "SiteName"] <- "St Jerome Bilingual School,  Station Road"
deployment_md[175, "Latitude"] <- "51.58533"
deployment_md[175, "Longitude"] <- "-0.3326874"
deployment_md[175, "SiteClassification"] <- "Roadside"
deployment_md[175, "HeadHeight"] <- "3.1"
deployment_md[175, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-176, ]
rownames(deployment_md) <- NULL

deployment_md[178, "SiteName"] <- "Shackleton Lane, Fulwell"
deployment_md[178, "Latitude"] <- "51.43209"
deployment_md[178, "Longitude"] <- "-0.3419193"
deployment_md[178, "SiteClassification"] <- "Roadside"
deployment_md[178, "HeadHeight"] <- "2.4"
deployment_md[178, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-179, ]
rownames(deployment_md) <- NULL

deployment_md[181, "SiteName"] <- "High St,  Hampton Wick"
deployment_md[181, "Latitude"] <- "51.4122"
deployment_md[181, "Longitude"] <- "-0.31075"
deployment_md[181, "SiteClassification"] <- "Roadside"
deployment_md[181, "HeadHeight"] <- "2.4"
deployment_md[181, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-182, ]
rownames(deployment_md) <- NULL

deployment_md[184, "SiteName"] <- "Petersham Rd,  opp Russell School,  TW10"
deployment_md[184, "Latitude"] <- "51.44449"
deployment_md[184, "Longitude"] <- "-0.3026678"
deployment_md[184, "SiteClassification"] <- "Roadside"
deployment_md[184, "HeadHeight"] <- "2.5"
deployment_md[184, "ToRoad"] <- "1"

deployment_md <- deployment_md[-185, ]
rownames(deployment_md) <- NULL

deployment_md[185, "SiteName"] <- "Sheen Lane,  south of j/w Vernon Rd,  nr zebra crossing, SW14"
deployment_md[185, "Latitude"] <- "51.46703"
deployment_md[185, "Longitude"] <- "-0.2667142"
deployment_md[185, "SiteClassification"] <- "Roadside"
deployment_md[185, "HeadHeight"] <- "2.5"
deployment_md[185, "ToRoad"] <- "0.4"

deployment_md <- deployment_md[-186, ]
rownames(deployment_md) <- NULL

deployment_md[186, "SiteName"] <- "Rocks Lane,  opp houses, SW13"
deployment_md[186, "Latitude"] <- "51.47373"
deployment_md[186, "Longitude"] <- "-0.2380387"
deployment_md[186, "SiteClassification"] <- "Roadside"
deployment_md[186, "HeadHeight"] <- "2.55"
deployment_md[186, "ToRoad"] <- "0.9"

deployment_md <- deployment_md[-187, ]
rownames(deployment_md) <- NULL

deployment_md[187, "SiteName"] <- "Richmond Pk,  Richmond Gate,  top of Star and Garter Hill"
deployment_md[187, "Latitude"] <- "51.45024"
deployment_md[187, "Longitude"] <- "-0.2966329"
deployment_md[187, "SiteClassification"] <- "Roadside"
deployment_md[187, "HeadHeight"] <- "2.5"
deployment_md[187, "ToRoad"] <- "0.95"

deployment_md <- deployment_md[-188, ]
rownames(deployment_md) <- NULL

deployment_md[188, "SiteName"] <- "Queens School,  Cumberland Rd"
deployment_md[188, "Latitude"] <- "51.48102"
deployment_md[188, "Longitude"] <- "-0.2846491"
deployment_md[188, "SiteClassification"] <- "Roadside"
deployment_md[188, "HeadHeight"] <- "2.45"
deployment_md[188, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-189, ]
rownames(deployment_md) <- NULL

deployment_md[189, "SiteName"] <- "Lwr Rich Rd,  Chertsy Corner,  nr j/w houses,  opp cnr of Chtsy Ct,  VS nr Kingsway"
deployment_md[189, "Latitude"] <- "51.46899"
deployment_md[189, "Longitude"] <- "-0.2758853"
deployment_md[189, "SiteClassification"] <- "Roadside"
deployment_md[189, "HeadHeight"] <- "2.1"
deployment_md[189, "ToRoad"] <- "3.8"

deployment_md <- deployment_md[-190, ]
rownames(deployment_md) <- NULL

deployment_md[190, "SiteName"] <- "High St, Barnes,  nr j/w St Anns Rd, SW13"
deployment_md[190, "Latitude"] <- "51.47381"
deployment_md[190, "Longitude"] <- "-0.2493043"
deployment_md[190, "SiteClassification"] <- "Roadside"
deployment_md[190, "HeadHeight"] <- "2.4"
deployment_md[190, "ToRoad"] <- "0.9"

deployment_md <- deployment_md[-191, ]
rownames(deployment_md) <- NULL

deployment_md[191, "SiteName"] <- "Burtons Road, (north side - relocated) Hampton Hill,  nr j/w High St"
deployment_md[191, "Latitude"] <- "51.42997"
deployment_md[191, "Longitude"] <- "-0.3539078"
deployment_md[191, "SiteClassification"] <- "Roadside"
deployment_md[191, "HeadHeight"] <- "2.5"
deployment_md[191, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-192, ]
rownames(deployment_md) <- NULL

deployment_md[192, "SiteName"] <- "High St, Mortlake,  opp j/w Vineyard Path, SW14"
deployment_md[192, "Latitude"] <- "51.45206"
deployment_md[192, "Longitude"] <- "-0.2651753"
deployment_md[192, "SiteClassification"] <- "Roadside"
deployment_md[192, "HeadHeight"] <- "2.4"
deployment_md[192, "ToRoad"] <- "0.6"

deployment_md <- deployment_md[-193, ]
rownames(deployment_md) <- NULL

deployment_md[193, "SiteName"] <- "Kew Rd,  before j/w South Circ/ Kew Green"
deployment_md[193, "Latitude"] <- "51.48261"
deployment_md[193, "Longitude"] <- "-0.2870275"
deployment_md[193, "SiteClassification"] <- "Roadside"
deployment_md[193, "HeadHeight"] <- "2.6"
deployment_md[193, "ToRoad"] <- "1.3"

deployment_md <- deployment_md[-194, ]
rownames(deployment_md) <- NULL

deployment_md[194, "SiteName"] <- "Harlington High Street, Hillingdon"
deployment_md[194, "Latitude"] <- "51.48215"
deployment_md[194, "Longitude"] <- "-0.43416"
deployment_md[194, "SiteClassification"] <- "Roadside"
deployment_md[194, "HeadHeight"] <- "2.4"
deployment_md[194, "ToRoad"] <- "1.5"

deployment_md <- deployment_md[-195, ]
rownames(deployment_md) <- NULL

deployment_md[195, "SiteName"] <- "Rumi Mosque, Enfield"
deployment_md[195, "Latitude"] <- "51.62064"
deployment_md[195, "Longitude"] <- "-0.06231"
deployment_md[195, "SiteClassification"] <- "Urban Background"
deployment_md[195, "HeadHeight"] <- "4"
deployment_md[195, "ToRoad"] <- "7"

deployment_md <- deployment_md[-196, ]
rownames(deployment_md) <- NULL

deployment_md[196, "SiteName"] <- "Kingston Hospital, Kingston"
deployment_md[196, "Latitude"] <- "51.41431"
deployment_md[196, "Longitude"] <- "-0.2810862"
deployment_md[196, "SiteClassification"] <- "Roadside"
deployment_md[196, "HeadHeight"] <- "3"
deployment_md[196, "ToRoad"] <- "0.4"

deployment_md <- deployment_md[-197, ]
rownames(deployment_md) <- NULL

deployment_md[197, "SiteName"] <- "Royal Marsden, Sutton"
deployment_md[197, "Latitude"] <- "51.34322"
deployment_md[197, "Longitude"] <- "-0.1909304"
deployment_md[197, "SiteClassification"] <- "Roadside"
deployment_md[197, "HeadHeight"] <- "3"
deployment_md[197, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-198, ]
rownames(deployment_md) <- NULL

deployment_md[198, "SiteName"] <- "St. Helier's Hospital, Sutton"
deployment_md[198, "Latitude"] <- "51.3806"
deployment_md[198, "Longitude"] <- "-0.182803"
deployment_md[198, "SiteClassification"] <- "Roadside"
deployment_md[198, "HeadHeight"] <- "3"
deployment_md[198, "ToRoad"] <- "1"

deployment_md <- deployment_md[-199, ]
rownames(deployment_md) <- NULL

deployment_md[271, "SiteName"] <- "A316,  Chertsey Court,  Clifford Ave,  nr Chertsey Corner,  SW14 8AD"
deployment_md[271, "Latitude"] <- "51.46974"
deployment_md[271, "Longitude"] <- "-0.2750381"
deployment_md[271, "SiteClassification"] <- "Roadside"
deployment_md[271, "HeadHeight"] <- "2.8"
deployment_md[271, "ToRoad"] <- "1.7"

deployment_md <- deployment_md[-272, ]
rownames(deployment_md) <- NULL

deployment_md[273, "SiteName"] <- "Colliers Wood,  80-82 High Street,  London SW19 2BY"
deployment_md[273, "Latitude"] <- "51.42028"
deployment_md[273, "Longitude"] <- "-0.1760002"
deployment_md[273, "SiteClassification"] <- "Roadside"
deployment_md[273, "HeadHeight"] <- "2.8"
deployment_md[273, "ToRoad"] <- "1.8"

deployment_md <- deployment_md[-274, ]
rownames(deployment_md) <- NULL

deployment_md[319, "SiteName"] <- "Royal Brompton Hospital,  Fulham Wing"
deployment_md[319, "Latitude"] <- "51.49053"
deployment_md[319, "Longitude"] <- "-0.173639"
deployment_md[319, "SiteClassification"] <- "Roadside"
deployment_md[319, "HeadHeight"] <- "2.5"
deployment_md[319, "ToRoad"] <- "2"

deployment_md <- deployment_md[-320, ]
rownames(deployment_md) <- NULL

deployment_md[346, "SiteName"] <- "Acer Avenue, Rainham"
deployment_md[346, "Latitude"] <- "51.52089"
deployment_md[346, "Longitude"] <- "0.2174496"
deployment_md[346, "SiteClassification"] <- "Urban Background"
deployment_md[346, "HeadHeight"] <- "2.35"
deployment_md[346, "ToRoad"] <- "17.7"

deployment_md <- deployment_md[-347, ]
rownames(deployment_md) <- NULL

deployment_md[347, "SiteName"] <- "King Edwards Ave, Rainham"
deployment_md[347, "Latitude"] <- "51.52274"
deployment_md[347, "Longitude"] <- "0.2145339"
deployment_md[347, "SiteClassification"] <- "Urban Background"
deployment_md[347, "HeadHeight"] <- "2.85"
deployment_md[347, "ToRoad"] <- "30"

deployment_md <- deployment_md[-348, ]
rownames(deployment_md) <- NULL

deployment_md[415, "SiteName"] <- "Nelson Primary School,  Nelson Rd TW2"
deployment_md[415, "Latitude"] <- "51.45288"
deployment_md[415, "Longitude"] <- "-0.361776"
deployment_md[415, "SiteClassification"] <- "Roadside"
deployment_md[415, "HeadHeight"] <- "2.5"
deployment_md[415, "ToRoad"] <- "1.9"

deployment_md <- deployment_md[-416, ]
rownames(deployment_md) <- NULL

deployment_md[416, "SiteName"] <- "The Vineyard Schl,  Friars Stile Rd,  TW10 6NE"
deployment_md[416, "Latitude"] <- "51.45474"
deployment_md[416, "Longitude"] <- "-0.29799"
deployment_md[416, "SiteClassification"] <- "Roadside"
deployment_md[416, "HeadHeight"] <- "2.5"
deployment_md[416, "ToRoad"] <- "0.5"

deployment_md <- deployment_md[-417, ]
rownames(deployment_md) <- NULL

deployment_md[417, "SiteName"] <- "Thomson House Sch,  Sheen Lane, SW14"
deployment_md[417, "Latitude"] <- "51.46833"
deployment_md[417, "Longitude"] <- "-0.266484"
deployment_md[417, "SiteClassification"] <- "Roadside"
deployment_md[417, "HeadHeight"] <- "2.4"
deployment_md[417, "ToRoad"] <- "1.5"

deployment_md <- deployment_md[-418, ]
rownames(deployment_md) <- NULL

deployment_md[418, "SiteName"] <- "St Richards CE Prim Schl,  Ashburnham Rd,  TW10 7NL"
deployment_md[418, "Latitude"] <- "51.43605"
deployment_md[418, "Longitude"] <- "-0.3189576"
deployment_md[418, "SiteClassification"] <- "Roadside"
deployment_md[418, "HeadHeight"] <- "2.3"
deployment_md[418, "ToRoad"] <- "1.6"

deployment_md <- deployment_md[-419, ]
rownames(deployment_md) <- NULL

deployment_md[419, "SiteName"] <- "Twickenham Primary Academy,  Heathgate House,  2-4 The Green Twickenham TW2 6QF"
deployment_md[419, "Latitude"] <- "51.44477"
deployment_md[419, "Longitude"] <- "-0.3401068"
deployment_md[419, "SiteClassification"] <- "Roadside"
deployment_md[419, "HeadHeight"] <- "2.3"
deployment_md[419, "ToRoad"] <- "2"

deployment_md <- deployment_md[-420, ]
rownames(deployment_md) <- NULL

deployment_md[420, "SiteName"] <- "Archdeacon Cambridge's CofE Prim Sch,  The Green,  Twickenham TW2 5TU"
deployment_md[420, "Latitude"] <- "51.44279"
deployment_md[420, "Longitude"] <- "-0.34248"
deployment_md[420, "SiteClassification"] <- "Roadside"
deployment_md[420, "HeadHeight"] <- "2.25"
deployment_md[420, "ToRoad"] <- "3.1"

deployment_md <- deployment_md[-421, ]
rownames(deployment_md) <- NULL

deployment_md[421, "SiteName"] <- "Hampton Hill Jnrs,  St James's Ave,  Hampton Hill TW12"
deployment_md[421, "Latitude"] <- "51.42751"
deployment_md[421, "Longitude"] <- "-0.360565"
deployment_md[421, "SiteClassification"] <- "Roadside"
deployment_md[421, "HeadHeight"] <- "2.5"
deployment_md[421, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-422, ]
rownames(deployment_md) <- NULL

deployment_md[422, "SiteName"] <- "Chase Bridge Primary School, Kneller Rd, TW2"
deployment_md[422, "Latitude"] <- "51.45374"
deployment_md[422, "Longitude"] <- "-0.345241"
deployment_md[422, "SiteClassification"] <- "Roadside"
deployment_md[422, "HeadHeight"] <- "2.7"
deployment_md[422, "ToRoad"] <- "0.3"

deployment_md <- deployment_md[-423, ]
rownames(deployment_md) <- NULL

deployment_md[423, "SiteName"] <- "Richmond bus garage,  Wakefield Rd,  Richmond"
deployment_md[423, "Latitude"] <- "51.45935"
deployment_md[423, "Longitude"] <- "-0.304159"
deployment_md[423, "SiteClassification"] <- "Roadside"
deployment_md[423, "HeadHeight"] <- "2.4"
deployment_md[423, "ToRoad"] <- "1.6"

deployment_md <- deployment_md[-424, ]
rownames(deployment_md) <- NULL

deployment_md[425, "SiteName"] <- "Aberfeldy, Poplar"
deployment_md[425, "Latitude"] <- "51.51374"
deployment_md[425, "Longitude"] <- "-0.006984306"
deployment_md[425, "SiteClassification"] <- "Roadside"
deployment_md[425, "HeadHeight"] <- "2.82"
deployment_md[425, "ToRoad"] <- "0.25"

deployment_md <- deployment_md[-426, ]
rownames(deployment_md) <- NULL

deployment_md[434, "SiteName"] <- "Harris Primary Academy,  East Dulwich"
deployment_md[434, "Latitude"] <- "51.45533"
deployment_md[434, "Longitude"] <- "-0.07651456"
deployment_md[434, "SiteClassification"] <- "Roadside"
deployment_md[434, "HeadHeight"] <- "3"
deployment_md[434, "ToRoad"] <- "5"

deployment_md <- deployment_md[-435, ]
rownames(deployment_md) <- NULL

deployment_md[442, "SiteName"] <- "Rosendale Primary School,  Norwood"
deployment_md[442, "Latitude"] <- "51.44637"
deployment_md[442, "Longitude"] <- "-0.09777512"
deployment_md[442, "SiteClassification"] <- "Urban Background"
deployment_md[442, "HeadHeight"] <- "2"
deployment_md[442, "ToRoad"] <- "40"

deployment_md <- deployment_md[-443, ]
rownames(deployment_md) <- NULL

deployment_md[453, "SiteName"] <- "St Matthew's Road, Brixton"
deployment_md[453, "Latitude"] <- "51.45938"
deployment_md[453, "Longitude"] <- "-0.1158274"
deployment_md[453, "SiteClassification"] <- "Urban Background"
deployment_md[453, "HeadHeight"] <- "3.4"
deployment_md[453, "ToRoad"] <- "13.5"

deployment_md <- deployment_md[-454, ]
rownames(deployment_md) <- NULL

deployment_md[454, "SiteName"] <- "Florence Road,  Lewisham"
deployment_md[454, "Latitude"] <- "51.47314"
deployment_md[454, "Longitude"] <- "-0.03053842"
deployment_md[454, "SiteClassification"] <- "Roadside"
deployment_md[454, "HeadHeight"] <- "2.5"
deployment_md[454, "ToRoad"] <- "1.5"

deployment_md <- deployment_md[-455, ]
rownames(deployment_md) <- NULL

deployment_md[455, "SiteName"] <- "Ashmead School, SE8"
deployment_md[455, "Latitude"] <- "51.46874"
deployment_md[455, "Longitude"] <- "-0.02601487"
deployment_md[455, "SiteClassification"] <- "Roadside"
deployment_md[455, "HeadHeight"] <- "1.8"
deployment_md[455, "ToRoad"] <- "10"

deployment_md <- deployment_md[-456, ]
rownames(deployment_md) <- NULL

deployment_md[527, "SiteName"] <- "Falcon Building, Bloomsbury"
deployment_md[527, "Latitude"] <- "51.52002"
deployment_md[527, "Longitude"] <- "-0.1206287"
deployment_md[527, "SiteClassification"] <- "Urban Background"
deployment_md[527, "HeadHeight"] <- "2"
deployment_md[527, "ToRoad"] <- "11"

deployment_md <- deployment_md[-528, ]
rownames(deployment_md) <- NULL

deployment_md[528, "SiteName"] <- "Church Grove,  Ladywell"
deployment_md[528, "Latitude"] <- "51.45757"
deployment_md[528, "Longitude"] <- "-0.01739958"
deployment_md[528, "SiteClassification"] <- "Urban Background"
deployment_md[528, "HeadHeight"] <- "5"
deployment_md[528, "ToRoad"] <- "0"

deployment_md <- deployment_md[-529, ]
rownames(deployment_md) <- NULL

deployment_md[563, "SiteName"] <- "Holy Trinity CE Prim Sch,  Carrington Rd,  TW10 5AA"
deployment_md[563, "Latitude"] <- "51.46325"
deployment_md[563, "Longitude"] <- "-0.2857711"
deployment_md[563, "SiteClassification"] <- "Roadside"
deployment_md[563, "HeadHeight"] <- "2.5"
deployment_md[563, "ToRoad"] <- "0.7"

deployment_md <- deployment_md[-564, ]
rownames(deployment_md) <- NULL

# write.csv(deployment_md, "DeploymentSitesMetadata_PreProcessed.csv", row.names = FALSE)

deployment_md_true <- read.csv("Data/DeploymentSitesMetadata_PreProcessed.csv")

View(deployment_md_true)
Crime_clean = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean.csv")

#Vict.Age.New
Crime_clean_recode <- Crime_clean_recode %>% 
  mutate(Vict.AgeGroup = ifelse(Vict.Age <= 23, "Age1",
                               ifelse(Vict.Age <= 32, "Age2",
                                      ifelse(Vict.Age <= 44, "Age3",
                                             ifelse(Vict.Age <= 55, "Age4",
                                                ifelse(Vict.Age <= 99, "Age5",NA))))))

#Vict.Descent.New
Asia = c("A","C","K","J","F","L","V","Z")
Europe = c("H","D","G","U")
Other = c("O","S","I","P")
Crime_clean_recode = Crime_clean_recode %>% 
  mutate(Vict.DescentGroup = ifelse(Vict.Descent %in% Asia,"Asia",
                                   ifelse(Vict.Descent %in% Europe,"Europe", 
                                          ifelse(Vict.Descent %in% Other,"Other",
                                                 ifelse(Vict.Descent == "B","Black",
                                                        ifelse(Vict.Descent == "W","White","Error" ))))))

#TimeOcc.New
Crime_clean_recode <- Crime_clean_recode %>% 
  mutate(TimeGroup =   ifelse(TIME.OCC <= 300, "Time1",
                              ifelse(TIME.OCC <= 800, "Time2",
                                     ifelse(TIME.OCC <= 1100, "Time3",
                                            ifelse(TIME.OCC <= 1300, "Time4",
                                                   ifelse(TIME.OCC <= 1500, "Time5",
                                                          ifelse(TIME.OCC <= 1700, "Time6",
                                                                 ifelse(TIME.OCC <= 1900, "Time7",
                                                                        ifelse(TIME.OCC <= 2100, "Time8",
                                                                               ifelse(TIME.OCC <= 2400, "Time9"))))))))))
                                                                                                                   



#Premis.CD/Premis.Desc						
road = c("STREET","VEHICLE, PASSENGER/TRUCK","VEHICLE, PASSENGER/TRUCK","DRIVEWAY","FREEWAY","BUS STOP/LAYOVER (ALSO QUERY 124)","BUS-CHARTER/PRIVATE","BUS, SCHOOL, CHURCH","TAXI","DRIVE THRU*","VALET","CATERING/ICE CREAM TRUCK","TUNNEL","TRUCK, COMMERICAL","OTHER INTERSTATE, CHARTER BUS","DRIVE THRU BANKING (WINDOW)*")
sidewalk =c("SIDEWALK","ALLEY","BUS STOP","PEDESTRIAN OVERCROSSING")
parking = c("PARKING LOT","GARAGE/CARPORT","PARKING UNDERGROUND/BUILDING","ORANGE LINE PARKING LOT")
residence = c("SINGLE FAMILY DWELLING","MULTI-UNIT DWELLING (APARTMENT, DUPLEX, ETC)","HOTEL","OTHER RESIDENCE","PORCH, RESIDENTIAL","MOTEL","CONDOMINIUM/TOWNHOUSE","NURSING/CONVALESCENT/RETIREMENT HOME","GROUP HOME","MEDICAL MARIJUANA FACILITIES/BUSINESSES","APARTMENT/CONDO COMMON LAUNDRY ROOM","TRANSITIONAL HOUSING/HALFWAY HOUSE","SINGLE RESIDENCE OCCUPANCY (SRO'S) LOCATIONS","TRANSIENT ENCAMPMENT","BALCONY*","HOSPICE")
Company_area = c("OTHER BUSINESS","OFFICE BUILDING/OFFICE","CONSTRUCTION SITE","DIY CENTER (LOWE'S,HOME DEPOT,OSH,CONTRACTORS WAREHOUSE)","STORAGE SHED","WAREHOUSE","CONVENTION CENTER","MANUFACTURING COMPANY","FACTORY","TOOL SHED*","VEHICLE STORAGE LOT (CARS, TRUCKS, RV'S, BOATS, TRAILERS, ETC.)","GARMENT MANUFACTURER","WATER FACILITY","OIL REFINERY","ENERGY PLANT/FACILITY","FINANCE COMPANY","CHEMICAL STORAGE/MANUFACTURING PLANT","NUCLEAR FACILITY")
public_area = c("PARK/PLAYGROUND","YARD (RESIDENTIAL/BUSINESS)","TRANSPORTATION FACILITY (AIRPORT)","PUBLIC STORAGE","GOVERNMENT FACILITY (FEDERAL,STATE, COUNTY & CITY)","MTA BUS","BEACH","PROJECT/TENEMENT/PUBLIC HOUSING","PATIO*","MAIL BOX","THE GROVE","PUBLIC RESTROOM(INDOORS-INSIDE)","DODGER STADIUM","LA UNION STATION (NOT LINE SPECIFIC)","PUBLIC RESTROOM/OUTSIDE*","VACANT LOT","TOW YARD*","AIRCRAFT","BUS DEPOT/TERMINAL, OTHER THAN MTA","TERMINAL, OTHER THAN MTA","POOL-PUBLIC/OUTDOOR OR INDOOR*","COLISEUM","MTA - RED LINE - WESTLAKE/MACARTHUR PARK","MTA - RED LINE - UNION STATION","MTA - RED LINE - NORTH HOLLYWOOD","MUNICIPAL BUS LINE INCLUDES LADOT/DASH","MTA - RED LINE - 7TH AND METRO CENTER","TRAIN TRACKS","MTA - RED LINE - HOLLYWOOD/VINE","MTA PROPERTY OR PARKING LOT","MTA - RED LINE - HOLLYWOOD/HIGHLAND","TRAIN DEPOT/TERMINAL, OTHER THAN MTA","MTA - RED LINE - WILSHIRE/VERMONT","SPORTS ARENA","METROLINK TRAIN","MTA - RED LINE - PERSHING SQUARE","MTA - RED LINE - VERMONT/BEVERLY","MTA - RED LINE - VERMONT/SANTA MONICA","MTA - BLUE LINE - 7TH AND METRO CENTER","MTA - ORANGE LINE - NORTH HOLLYWOOD","MTA - EXPO LINE - EXPO/WESTERN","MTA - RED LINE - VERMONT/SUNSET","MTA - PURPLE LINE - WILSHIRE/VERMONT","TRAIN, OTHER THAN MTA (ALSO QUERY 809/810/811)","OTHER RR TRAIN (UNION PAC, SANTE FE ETC","MTA - RED LINE - HOLLYWOOD/WESTERN","MTA - GREEN LINE - HARBOR FWY","MTA - GREEN LINE - AVALON","MTA - RED LINE - UNIVERSAL CITY/STUDIO CITY","MTA - PURPLE LINE - WILSHIRE/WESTERN","MTA - BLUE LINE - 103RD/WATTS TOWERS","MTA - EXPO LINE - EXPO/LA BREA","MTA - EXPO LINE - EXPO/CRENSHAW","MTA - BLUE LINE - WASHINGTON","MTA - EXPO LINE - LA CIENEGA/JEFFERSON","MTA - EXPO LINE - 7TH AND METRO CENTER","MTA - EXPO LINE - FARMDALE","MTA - EXPO LINE - EXPO/BUNDY","MTA - GOLD LINE - UNION STATION","MTA - EXPO LINE - EXPO/VERMONT","MTA - EXPO LINE - LATTC/ORTHO INSTITUTE","MTA - BLUE LINE - PICO","MTA - ORANGE LINE - CANOGA","MTA - GOLD LINE - MARIACHI PLAZA","MTA - GOLD LINE - HIGHLAND PARK","REDLINE SUBWAY PLATFORM","MTA - RED LINE - CIVIC CENTER/GRAND PARK","MTA - PURPLE LINE - WILSHIRE/NORMANDIE","MTA - EXPO LINE - EXPO/SEPULVEDA","MTA - GREEN LINE - AVIATION/LAX","MTA - GOLD LINE - SOTO","MTA - EXPO LINE - JEFFERSON/USC","MTA - BLUE LINE - VERNON","MTA - BLUE LINE - SAN PEDRO","MTA - ORANGE LINE - VAN NUYS","MTA - GOLD LINE - LINCOLN/CYPRESS","MTA - EXPO LINE - PICO","MTA - EXPO LINE - PALMS","MTA - PURPLE LINE - UNION STATION","MTA - ORANGE LINE - CHATSWORTH","MTA - EXPO LINE - EXPO PARK/USC","AMTRAK TRAIN","MTA - SILVER LINE - HARBOR GATEWAY TRANSIT CTR","MTA - PURPLE LINE - PERSHING SQUARE","MTA - ORANGE LINE - BALBOA","MTA - GOLD LINE - CHINATOWN","MTA - BLUE LINE - GRAND/LATTC","MTA - ORANGE LINE - RESEDA","MTA - GOLD LINE - HERITAGE SQ","MTA - EXPO LINE - WESTWOOD/RANCHO PARK","MTA - PURPLE LINE - WESTLAKE/MACARTHUR PARK","MTA - GOLD LINE - SOUTHWEST MUSEUM","MTA - GOLD LINE - PICO/ALISO","MTA - ORANGE LINE - SHERMAN WAY","MTA - GOLD LINE - LITTLE TOKYO/ARTS DISTRICT","MTA - ORANGE LINE - PIERCE COLLEGE","MTA - GOLD LINE - INDIANA","MUSCLE BEACH","REDLINE (SUBWAY TRAIN)","MTA - SILVER LINE - DOWNTOWN STREET STOPS","MTA - PURPLE LINE - 7TH AND METRO CENTER","BLUE LINE (ABOVE GROUND SURFACE TRAIN)","MTA - ORANGE LINE - DE SOTO","REDLINE SUBWAY RAIL CAR (INSIDE TRAIN)","TRAM/STREETCAR(BOXLIKE WAG ON RAILS)*","MTA - SILVER LINE - UNION STATION","MTA - SILVER LINE - PACIFIC COAST HWY","MTA - SILVER LINE - HARBOR FWY","MTA - PURPLE LINE - CIVIC CENTER/GRAND PARK","MTA - ORANGE LINE - ROSCOE","MTA - ORANGE LINE - NORDHOFF","MTA - SILVER LINE - 37TH ST/USC","MTA - ORANGE LINE - WOODLEY","MTA - ORANGE LINE - SEPULVEDA","MTA - SILVER LINE - MANCHESTER","MTA - SILVER LINE - CAL STATE LA","MTA - ORANGE LINE - TAMPA","MTA - ORANGE LINE - WARNER CTR","MTA - ORANGE LINE - LAUREL CANYON","REDLINE SUBWAY TUNNEL","MTA - SILVER LINE - LAC/USC MEDICAL CENTER","MTA - SILVER LINE - SLAUSON","MTA - ORANGE LINE - WOODMAN","REDLINE SUBWAY MEZZANINE")
department_store = c("DEPARTMENT STORE","RESTAURANT/FAST FOOD","MARKET","OTHER STORE","DRUG STORE","MINI-MART","CLOTHING STORE","LIQUOR STORE","CELL PHONE STORE","SHOPPING MALL (COMMON AREA)","COFFEE SHOP (STARBUCKS, COFFEE BEAN, PEET'S, ETC.)","DISCOUNT STORE (99 CENT,DOLLAR,ETC.","MEMBERSHIP STORE (COSTCO,SAMS CLUB)*","AUTO SUPPLY STORE*","JEWELRY STORE","BEAUTY SUPPLY STORE","TOBACCO SHOP","PHARMACY INSIDE STORE OR SUPERMARKET*","THE BEVERLY CONNECTION","ELECTRONICS STORE (IE:RADIO SHACK, ETC.)","THE BEVERLY CENTER","PET STORE","GUN/SPORTING GOODS","PAWN SHOP","OPTICAL OFFICE INSIDE STORE OR SUPERMARKET*","FURNITURE STORE","VIDEO RENTAL STORE","BOOK STORE","SURPLUS SURVIVAL STORE")
educational = c("HIGH SCHOOL","JUNIOR HIGH SCHOOL","ELEMENTARY SCHOOL","COLLEGE/JUNIOR COLLEGE/UNIVERSITY","SPECIALTY SCHOOL/OTHER","MISSIONS/SHELTERS","PRIVATE SCHOOL/PRESCHOOL","TRADE SCHOOL (MEDICAL-TECHNICAL-BUSINESS)*")
service = c("GAS STATION","HEALTH SPA/GYM","NIGHT CLUB (OPEN EVENINGS ONLY)","HOSPITAL","CHURCH/CHAPEL (CHANGED 03-03 FROM CHURCH/TEMPLE)","LAUNDROMAT","BAR/COCKTAIL/NIGHTCLUB","MEDICAL/DENTAL OFFICES","LIBRARY","AUTO REPAIR SHOP","BANK","BEAUTY/BARBER SHOP","POLICE FACILITY","BAR/SPORTS BAR (OPEN DAY & NIGHT)","CAR WASH","THEATRE/MOVIE","NAIL SALON","CLEANER/LAUNDROMAT","DETENTION/JAIL FACILITY","NURSERY/FLOWER SHOP","SYNAGOGUE/TEMPLE","ENTERTAINMENT/COMEDY CLUB (OTHER)","TV/RADIO/APPLIANCE","POST OFFICE","STUDIO (FILM/PHOTOGRAPHIC/MUSIC)","COMPUTER SERVICES/REPAIRS/SALES","GOLF COURSE*","SEX ORIENTED/BOOK STORE/STRIP CLUB/GENTLEMAN'S CLUB","MASSAGE PARLOR","BOWLING ALLEY*","FIRE STATION","DELIVERY SERVICE (FED EX, UPS, COURIERS,COURIER SERVICE)*","MUSEUM","DAY CARE/ADULTS*","DAY CARE/CHILDREN*","TATTOO PARLOR*","SPORTS VENUE, OTHER","SKATEBOARD FACILITY/SKATEBOARD PARK*","EQUIPMENT RENTAL","MOSQUE*","OTHER PLACE OF WORSHIP","VETERINARIAN/ANIMAL HOSPITAL","MASS GATHERING LOCATION","SKATING RINK*","ABORTION CLINIC/ABORTION FACILITY*","AMUSEMENT PARK*","METHADONE CLINIC","HOCKEY RINK/ICE HOCKEY","BANKING INSIDE MARKET-STORE *","HANDBALL COURTS")
other = c("OTHER PREMISE","OTHER/OUTSIDE","HARDWARE/BUILDING SUPPLY","MOBILE HOME/TRAILERS/CONSTRUCTION TRAILERS/RV'S/MOTORHOME","AUTO SALES LOT","SLIPS/DOCK/MARINA/BOAT","SWAP MEET","AUTOMATED TELLER MACHINE (ATM)","RECYCLING CENTER","FRAT HOUSE/SORORITY/DORMITORY","CHECK CASHING*","ABANDONED BUILDING ABANDONED HOUSE","ELEVATOR","VISION CARE FACILITY*","AUTO DEALERSHIP (CHEVY, FORD, BMW, MERCEDES, ETC.)","STAIRWELL*","CEMETARY*","STAPLES CENTER *","FOSTER HOME BOYS OR GIRLS*","UNDERPASS/BRIDGE*","RIVER BED*","SHORT-TERM VACATION RENTAL","ARCADE,GAME ROOM/VIDEO GAMES (EXAMPLE CHUCKIE CHEESE)*","GREYHOUND OR INTERSTATE BUS","TELECOMMUNICATION FACILITY/LOCATION","BASKETBALL COURTS","MORTUARY","7TH AND METRO CENTER (NOT LINE SPECIFIC)","RECORD-CD MUSIC/COMPUTER GAME STORE","SAVINGS & LOAN","CREDIT UNION","HIGH-RISE BUILDING","REDLINE ENTRANCE/EXIT","RETIRED (DUPLICATE) DO NOT USE THIS CODE","ABATEMENT LOCATION","DAM/RESERVOIR","PAY PHONE","CULTURAL SIGNIFICANCE/MONUMENT","TRASH CAN/TRASH DUMPSTER","ESCALATOR*","CYBERSPACE","SEWAGE FACILITY/PIPE","WEBSITE","HORSE RACING/SANTA ANITA PARK*","DEPT OF DEFENSE FACILITY","TACTICAL SIGNIFICANCE","HARBOR FRWY STATION (NOT LINE SPECIFIC)","BANK DROP BOX/MONEY DROP-OUTSIDE OF BANK*")
Crime_clean_recode = Crime_clean_recode %>% mutate(LocationGroup = ifelse(Premis.Desc %in% road,"road",
                                                                 ifelse(Premis.Desc %in% sidewalk,"sidewalk",
                                                                        ifelse(Premis.Desc %in% parking,"parking",
                                                                               ifelse(Premis.Desc %in% residence,"residence",
                                                                                      ifelse(Premis.Desc %in% Company_area,"Company_area",
                                                                                             ifelse(Premis.Desc %in% public_area,"public_area",
                                                                                                    ifelse(Premis.Desc %in% department_store,"department_store",
                                                                                                           ifelse(Premis.Desc %in% educational,"educational",
                                                                                                                  ifelse(Premis.Desc %in% service,"service",
                                                                                                                         ifelse(Premis.Desc %in% other,"other",NA)))))))))))

Crime_clean_recode %>% count(LocationGroup)
Crime_clean_recode %>% select (LocationGroup,Premis.Desc) %>% filter(LocationGroup = NA)


Artificial_weapons = c("BOTTLE","STICK","ROCK/THROWN OBJECT","CLUB/BAT","BLUNT INSTRUMENT","MACE/PEPPER SPRAY","PIPE/METAL PIPE","OTHER CUTTING INSTRUMENT","UNKNOWN TYPE CUTTING INSTRUMENT","SCREWDRIVER","BELT FLAILING INSTRUMENT/CHAIN","SCISSORS","CONCRETE BLOCK/BRICK","FIXED OBJECT","BOARD","GLASS","BRASS KNUCKLES","TIRE IRON","TOY GUN","ICE PICK","SWORD","DIRK/DAGGER","ROPE/LIGATURE","BLACKJACK","MARTIAL ARTS WEAPONS","SYRINGE")
Gun =  c("HAND GUN","SEMI-AUTOMATIC PISTOL","UNKNOWN FIREARM","REVOLVER","SIMULATED GUN","AIR PISTOL/REVOLVER/RIFLE/BB GUN","SHOTGUN","RIFLE","OTHER FIREARM","STUN GUN","AUTOMATIC WEAPON/SUB-MACHINE GUN","HECKLER & KOCH 93 SEMIAUTOMATIC ASSAULT RIFLE","ASSAULT WEAPON/UZI/AK47/ETC","SAWED OFF RIFLE/SHOTGUN","STARTER PISTOL/REVOLVER","SEMI-AUTOMATIC RIFLE","UNK TYPE SEMIAUTOMATIC ASSAULT RIFLE","UZI SEMIAUTOMATIC ASSAULT RIFLE","HECKLER & KOCH 91 SEMIAUTOMATIC ASSAULT RIFLE","RELIC FIREARM","MAC-10 SEMIAUTOMATIC ASSAULT WEAPON","ANTIQUE FIREARM","MAC-11 SEMIAUTOMATIC ASSAULT WEAPON","M-14 SEMIAUTOMATIC ASSAULT RIFLE","M1-1 SEMIAUTOMATIC ASSAULT RIFLE")
Knife =  c("KNIFE WITH BLADE 6INCHES OR LESS","OTHER KNIFE","FOLDING KNIFE","KITCHEN KNIFE","KNIFE WITH BLADE OVER 6 INCHES IN LENGTH","MACHETE","SWITCH BLADE","RAZOR BLADE","RAZOR","CLEAVER","BOWIE KNIFE","STRAIGHT RAZOR")
Other =  c("UNKNOWN WEAPON/OTHER WEAPON","VEHICLE","VERBAL THREAT","HAMMER","CAUSTIC CHEMICAL/POISON","PHYSICAL PRESENCE","SCALDING LIQUID","AXE","DEMAND NOTE","FIRE","LIQUOR/DRUGS","EXPLOXIVE DEVICE","BOMB THREAT","DOG/ANIMAL (SIC ANIMAL ON)","BOW AND ARROW")
StrongBody = c("STRONG-ARM (HANDS, FIST, FEET OR BODILY FORCE)")
Crime_clean_recode = Crime_clean_recode %>% mutate(WeaponGroup = ifelse(Weapon.Desc %in% Artificial_weapons,"Weapon1",
                                                                              ifelse(Weapon.Desc %in% Gun,"Weapon2",
                                                                                     ifelse(Weapon.Desc %in% Knife,"Weapon3",
                                                                                            ifelse(Weapon.Desc %in% Other,"Weapon4",
                                                                                                   ifelse(Weapon.Desc == "Non_Weapon","Non_Weapon",
                                                                                                          ifelse(Weapon.Desc %in% StrongBody,"Weapon5",
                                                                                                                  NA)))))))
Crime_clean_recode %>% count(WeaponGroup)                                                               
Crime_clean_recode %>% select (Weapon.Desc,WeaponGroup) %>% filter(WeaponGroup =NA)





Crime_clean_recode_select = Crime_clean_recode_select %>% mutate(LocationGroup = ifelse(LocationGroup == "road","location1",
                                                            ifelse(LocationGroup == "sidewalk","location2",
                                                                   ifelse(LocationGroup == "parking","location3",
                                                                          ifelse(LocationGroup == "residence","location4",
                                                                                 ifelse(LocationGroup == "Company_area","location5",
                                                                                        ifelse(LocationGroup == "public_area","location6",
                                                                                               ifelse(LocationGroup == "department_store","location7",
                                                                                                      ifelse(LocationGroup == "educational","location8",
                                                                                                             ifelse(LocationGroup == "service","location9",
                                                                                                                    ifelse(LocationGroup == "other","location10",NA)))))))))))
Crime_clean_recode_select = Crime_clean_recode_select %>% mutate(AREA.NAME = ifelse(AREA.NAME == "77th Street","Area1",
                                                          ifelse(AREA.NAME == "Central","Area2",
                                                                 ifelse(AREA.NAME == "Devonshire","Area3",
                                                                        ifelse(AREA.NAME == "Foothill","Area4",
                                                                               ifelse(AREA.NAME == "Harbor","Area5",
                                                                                      ifelse(AREA.NAME == "Hollenbeck","Area6",
                                                                                             ifelse(AREA.NAME == "Hollywood","Area7",
                                                                                                    ifelse(AREA.NAME == "Mission","Area8",
                                                                                                           ifelse(AREA.NAME == "N Hollywood","Area9",
                                                                                                                  ifelse(AREA.NAME == "Newton","Area10",
                                                                                                                         ifelse(AREA.NAME == "Northeast","Area11",
                                                                                                                                ifelse(AREA.NAME == "Olympic","Area12",
                                                                                                                                       ifelse(AREA.NAME == "Pacific","Area13",
                                                                                                                                              ifelse(AREA.NAME == "Rampart","Area14",
                                                                                                                                                     ifelse(AREA.NAME == "Southeast","Area15",
                                                                                                                                                            ifelse(AREA.NAME == "Southwest","Area16",
                                                                                                                                                                   ifelse(AREA.NAME == "Topanga","Area17",
                                                                                                                                                                          ifelse(AREA.NAME == "Van Nuys","Area18",
                                                                                                                                                                                 ifelse(AREA.NAME == "West LA","Area19",
                                                                                                                                                                                        ifelse(AREA.NAME == "West Valley","Area20",
                                                                                                                                                                                               ifelse(AREA.NAME == "Wilshire","Area21",NA))))))))))))))))))))))

write.csv(Crime_clean_recode,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_recode.csv")

#NoNA
Crime_clean_recode = read.csv("C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean.csv")
Crime_clean_recode_noNa = na.omit(Crime_clean_recode)
write.csv(Crime_clean_recode_noNa,"C:/Users/matages/Desktop/Project/Crm_csv/Crime_clean_recode_noNA.csv")




#frequency
frequency_TIME <- table(Crime_clean_recode$TimeGroup) ;frequency_TIME
frequency_Age <- table(Crime_clean_recode$Vict.AgeGroup) ;frequency_Age 
frequency_Descent<- table(Crime_clean_recode$Vict.DescentGroup) ;frequency_Descent
frequency_LocationGroup<- table(Crime_clean_recode$LocationGroup) ;frequency_LocationGroup


#prop
prop_TIME <- prop.table(table(Crime_clean_recode$TimeGroup)) ;prop_TIME
prop_Age <- prop.table(table(Crime_clean_recode$Vict.AgeGroup)) ;prop_Age
prop_Descent <- prop.table(table(Crime_clean_recode$Vict.DescentGroup)) ;prop_Descent
prop_LocationGroup <- prop.table(table(Crime_clean_recode$LocationGroup)) ;prop_LocationGroup







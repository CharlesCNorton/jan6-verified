
type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b



val add : int -> int -> int

val mul : int -> int -> int

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

type string =
| EmptyString
| String of ascii * string

type minute = int

val time_of_day : int -> int -> minute

val next_day_offset : minute

type capitolLocation =
| SenateWing
| HouseWing
| SenateChamber
| HouseChamber
| SpeakerLobby
| Rotunda
| CryptLevel
| StatuaryHall
| WestFront
| WestFrontUpper
| WestFrontLower
| LowerWestTerrace
| LowerWestTerraceTunnel
| EastFront
| EastRotundaDoors
| NorthBarricade
| SenateHideaway
| HouseGallery
| SenateGallery

type dCLocation =
| WhiteHouseEllipse
| CapitolGrounds
| CapitolBuilding of capitolLocation
| RNCHeadquarters
| DNCHeadquarters
| DCNationalGuardArmory
| Pentagon
| FBIHeadquarters

type lawEnforcementAgency =
| USCP
| MPD
| SecretService
| DCNG
| FBIAgency
| ParkPolice
| MarylandStatePolice
| VirginiaStatePolice

type actorCategory =
| Rioter
| LawEnforcement of lawEnforcementAgency
| ElectedOfficial
| ExecutiveBranchOfficial
| MilitaryOfficial
| Civilian
| MediaMember

type extremistGroup =
| OathKeepers
| ProudBoys
| ThreePercenters
| QAnon
| GrundyCountyMilitia
| NoGroupAffiliation

type convictedDefendant =
| StewartRhodes
| EnriqueTarrio
| KellyMeggs
| JessicaWatkins
| KennethHarrelson
| RobertoMinuta
| EdwardVallejo
| DavidMoerschel
| EthanNordean
| JosephBiggs
| ZacharyRehl
| DominicPezzola
| CharlesDonohoe
| JeremyBertino
| ThomasWebster
| JacobChansley
| RichardBarnett
| GuyReffitt
| PaulHodgkins
| PatrickMcCaughey
| ScottFairlamb
| RobertPalmer

type lawEnforcementOfficer =
| EugeneGoodman
| MichaelFanone
| DanielHodges
| AquilinoGonell
| HarryDunn
| CarolineEdwards
| MichaelByrd
| BrianSicknick
| HowardLiebengood
| JeffreySmith
| GuntherHashida
| KyleDeFretag
| WinstonPingeon
| JamesBlasingame
| SidneyHemby
| NoahRathbun
| PhusomNguyen
| JasonMastony
| AbdulkadirAbdi
| GeorgeDonigian
| WilliamBogner
| OfficerCE
| OfficerDC
| OfficerAW
| OfficerLostEye
| OfficerStabbed
| OfficerCrackedRibs
| OfficerDraggedLeg

type electedOfficialPerson =
| MikePence
| NancyPelosi
| ChuckSchumer
| MitchMcConnell
| KevinMcCarthy
| MittRomney
| JimJordan
| TedCruz
| JoshHawley
| PaulGosar
| MoEBrooks
| LouieGohmert

type executiveOfficialPerson =
| DonaldTrump
| MarkMeadows
| PatCipollone
| RudyGiuliani
| JohnEastman
| JeffreyClark
| KayleighMcEnany

type militaryOfficialPerson =
| ChristopherMiller
| RyanMcCarthy
| WilliamWalker
| MarkMilley
| CharlesFlynn
| WalterPiatt

type capitolSecurityPerson =
| StevenSund
| PaulIrving
| MichaelStenger

type civilianDecedent =
| AshliBabbitt
| RosanneBoyland
| KevinGreeson
| BenjaminPhillips

type actor =
| ConvictedActor of convictedDefendant
| OfficerActor of lawEnforcementOfficer
| ElectedActor of electedOfficialPerson
| ExecutiveActor of executiveOfficialPerson
| MilitaryActor of militaryOfficialPerson
| SecurityActor of capitolSecurityPerson
| DeceasedCivilian of civilianDecedent
| AnonymousRioter
| AnonymousOfficer of lawEnforcementAgency

type eventType =
| RallyBegins
| SpeechBegins
| SpeechEnds
| MarchInstruction
| JointSessionConvenes
| JointSessionRecesses
| JointSessionResumes
| CertificationCompletes
| ObjectionRaised
| ObjectionVoteFails
| BarricadeBreach
| ScaffoldTaken
| PoliceLineCollapse
| WindowBreach
| DoorBreach
| BuildingBreach
| RoomBreach
| RiotDeclared
| Evacuation
| OfficerAssault
| OfficerCrushed
| CivilianShot
| CivilianDeath
| OfficerStroke
| OfficerSuicide
| GuardRequested
| GuardDenied
| GuardAuthorized
| GuardArrives
| CapitolCleared
| CapitolSecured
| CurfewDeclared
| TweetPosted
| VideoPosted
| PhoneCall
| PipeBombDiscovered
| PipeBombNeutralized

type event = { event_time : minute; event_type : eventType;
               event_location : dCLocation; event_actor : actor option;
               event_source : string }

val t_0653 : minute

val t_1100 : minute

val t_1200 : minute

val t_1253 : minute

val t_1300 : minute

val t_1305 : minute

val t_1310 : minute

val t_1311 : minute

val t_1349 : minute

val t_1350 : minute

val t_1358 : minute

val t_1400 : minute

val t_1411 : minute

val t_1412 : minute

val t_1413 : minute

val t_1414 : minute

val t_1415 : minute

val t_1418 : minute

val t_1424 : minute

val t_1425 : minute

val t_1426 : minute

val t_1430 : minute

val t_1444 : minute

val t_1515 : minute

val t_1521 : minute

val t_1533 : minute

val t_1617 : minute

val t_1632 : minute

val t_1636 : minute

val t_1720 : minute

val t_1800 : minute

val t_2000 : minute

val t_2100 : minute

val t_next_0006 : minute

val t_next_0344 : minute

val ev_pipebomb_placed : event

val ev_rally_begins : event

val ev_trump_speech_begins : event

val ev_joint_session_convenes : event

val ev_pipebomb_rnc_found : event

val ev_speech_ends : event

val ev_march_instruction : event

val ev_guard_requested_sund : event

val ev_riot_declared : event

val ev_first_barricade : event

val ev_north_barricade : event

val ev_police_line_west : event

val ev_window_breach : event

val ev_building_breach_west : event

val ev_senate_recesses : event

val ev_pence_evacuated : event

val ev_goodman_redirects_romney : event

val ev_rioters_senate_floor : event

val ev_house_recesses : event

val ev_pence_tweet : event

val ev_pence_40ft : event

val ev_crypt_breach : event

val ev_mccarthy_call : event

val ev_rotunda_breach : event

val ev_babbitt_shot : event

val ev_tunnel_crush_hodges : event

val ev_tunnel_assault_fanone : event

val ev_pipebomb_rnc_neutralized : event

val ev_guard_authorized : event

val ev_pipebomb_dnc_neutralized : event

val ev_trump_video : event

val ev_guard_arrives : event

val ev_capitol_cleared : event

val ev_curfew : event

val ev_session_resumes : event

val ev_arizona_objection_fails : event

val ev_pennsylvania_objection_fails : event

val ev_certification_complete : event

val timeline : event list

type federalCharge =
| Charge_18USC2384
| Charge_18USC1512c2
| Charge_18USC1512k
| Charge_18USC111
| Charge_18USC231
| Charge_18USC1361
| Charge_18USC1752
| Charge_40USC5104
| Charge_18USC372
| Charge_18USC1519

type sentencingEnhancement =
| TerrorismEnhancement
| LeadershipEnhancement
| WeaponEnhancement
| VulnerableVictimEnhancement

type convictionStatus =
| Convicted
| Acquitted
| PleaAgreement
| Pardoned
| Commuted
| SentenceServing
| SentenceCompleted
| AwaitingTrial
| Fugitive

type trialOutcome =
| JuryVerdict
| GuiltyPlea
| BenchTrial
| Dismissed

type conviction = { conv_defendant : convictedDefendant;
                    conv_group : extremistGroup;
                    conv_charges : federalCharge list;
                    conv_verdict_date : string; conv_sentence_months : 
                    int; conv_enhancements : sentencingEnhancement list;
                    conv_trial_outcome : trialOutcome;
                    conv_status_jan2025 : convictionStatus }

val conv_rhodes : conviction

val conv_meggs : conviction

val conv_watkins : conviction

val conv_harrelson : conviction

val conv_tarrio : conviction

val conv_nordean : conviction

val conv_biggs : conviction

val conv_rehl : conviction

val conv_pezzola : conviction

val conv_chansley : conviction

val conv_barnett : conviction

val conv_reffitt : conviction

val seditious_convictions : conviction list

val all_convictions : conviction list

type causeOfDeath =
| Gunshot
| AcuteAmphetamineIntoxication
| HeartAttackCOD
| StrokeCOD
| Suicide

type mannerOfDeath =
| Homicide
| Accident
| Natural
| SuicideClass
| Undetermined

type decedent = { dec_name : (civilianDecedent, lawEnforcementOfficer) sum;
                  dec_category : actorCategory; dec_location : dCLocation;
                  dec_time : minute option; dec_cause : causeOfDeath;
                  dec_manner : mannerOfDeath; dec_days_after_jan6 : int;
                  dec_source : string }

val death_babbitt : decedent

val death_boyland : decedent

val death_greeson : decedent

val death_phillips : decedent

val death_sicknick : decedent

val death_liebengood : decedent

val death_smith : decedent

val death_hashida : decedent

val death_defreytag : decedent

val jan6_deaths : decedent list

val officer_suicides : decedent list

type uSState =
| Alabama
| Alaska
| Arizona
| Arkansas
| California
| Colorado
| Connecticut
| Delaware
| Florida
| Georgia
| Hawaii
| Idaho
| Illinois
| Indiana
| Iowa
| Kansas
| Kentucky
| Louisiana
| Maine
| Maryland
| Massachusetts
| Michigan
| Minnesota
| Mississippi
| Missouri
| Montana
| Nebraska
| Nevada
| NewHampshire
| NewJersey
| NewMexico
| NewYork
| NorthCarolina
| NorthDakota
| Ohio
| Oklahoma
| Oregon
| Pennsylvania
| RhodeIsland
| SouthCarolina
| SouthDakota
| Tennessee
| Texas
| Utah
| Vermont
| Virginia
| Washington
| WestVirginia
| Wisconsin
| Wyoming
| DistrictOfColumbia

type candidate2020 =
| JoeBiden
| DonaldTrumpCand

type stateElectoralVotes = { sev_state : uSState; sev_electoral_votes : 
                             int; sev_winner : candidate2020;
                             sev_certified : bool;
                             sev_objection_filed : bool;
                             sev_objection_sustained : bool;
                             sev_fake_electors_submitted : bool }

val ev_arizona : stateElectoralVotes

val ev_georgia : stateElectoralVotes

val ev_michigan : stateElectoralVotes

val ev_nevada : stateElectoralVotes

val ev_newmexico : stateElectoralVotes

val ev_pennsylvania : stateElectoralVotes

val ev_wisconsin : stateElectoralVotes

val contested_states : stateElectoralVotes list

type certificationState =
| NotConvened
| InSession
| Recessed
| Interrupted
| Resumed
| Certified

type electoralCertification = { cert_presiding_officer : electedOfficialPerson;
                                cert_session_start : minute;
                                cert_interruption_time : minute option;
                                cert_resumption_time : minute option;
                                cert_completion_time : minute option;
                                cert_final_state : certificationState }

val jan6_certification : electoralCertification

type requestStatus =
| Pending
| Denied
| Authorized
| Deployed

type guardRequest = { gr_requester : capitolSecurityPerson;
                      gr_request_time : minute;
                      gr_initial_response : requestStatus;
                      gr_authorization_time : minute option;
                      gr_authorizing_official : militaryOfficialPerson option;
                      gr_deployment_time : minute option }

val jan6_guard_request : guardRequest

type pipeBomb = { pb_location : dCLocation;
                  pb_placement_time : minute option;
                  pb_discovery_time : minute;
                  pb_neutralization_time : minute; pb_bomber_identified : 
                  bool }

val rnc_pipebomb : pipeBomb

val dnc_pipebomb : pipeBomb

val pipebombs : pipeBomb list


type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b



type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

type uint0 =
| Nil0
| D10 of uint0
| D11 of uint0
| D12 of uint0
| D13 of uint0
| D14 of uint0
| D15 of uint0
| D16 of uint0
| D17 of uint0
| D18 of uint0
| D19 of uint0
| Da of uint0
| Db of uint0
| Dc of uint0
| Dd of uint0
| De of uint0
| Df of uint0

type uint1 =
| UIntDecimal of uint
| UIntHexadecimal of uint0

val add : int -> int -> int

val mul : int -> int -> int

val tail_add : int -> int -> int

val tail_addmul : int -> int -> int -> int

val tail_mul : int -> int -> int

val of_uint_acc : uint -> int -> int

val of_uint : uint -> int

val of_hex_uint_acc : uint0 -> int -> int

val of_hex_uint : uint0 -> int

val of_num_uint : uint1 -> int

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
| JesseLeasure
| RobertGlover
| ChristopherMonroe
| ThomasBurgess
| MarkKeene
| OfficerCE
| OfficerDC
| OfficerAW
| OfficerLostEye
| OfficerStabbed
| OfficerCrackedRibs
| OfficerDraggedLeg
| USCP_001
| USCP_002
| USCP_003
| USCP_004
| USCP_005
| USCP_006
| USCP_007
| USCP_008
| USCP_009
| USCP_010
| USCP_011
| USCP_012
| USCP_013
| USCP_014
| USCP_015
| USCP_016
| USCP_017
| USCP_018
| USCP_019
| USCP_020
| USCP_021
| USCP_022
| USCP_023
| USCP_024
| USCP_025
| USCP_026
| USCP_027
| USCP_028
| USCP_029
| USCP_030
| USCP_031
| USCP_032
| USCP_033
| USCP_034
| USCP_035
| USCP_036
| USCP_037
| USCP_038
| USCP_039
| USCP_040
| USCP_041
| USCP_042
| USCP_043
| USCP_044
| USCP_045
| USCP_046
| USCP_047
| USCP_048
| USCP_049
| USCP_050
| USCP_051
| USCP_052
| USCP_053
| USCP_054
| USCP_055
| MPD_001
| MPD_002
| MPD_003
| MPD_004
| MPD_005
| MPD_006
| MPD_007
| MPD_008
| MPD_009
| MPD_010
| MPD_011
| MPD_012
| MPD_013
| MPD_014
| MPD_015
| MPD_016
| MPD_017
| MPD_018
| MPD_019
| MPD_020
| MPD_021
| MPD_022
| MPD_023
| MPD_024
| MPD_025
| MPD_026
| MPD_027
| MPD_028
| MPD_029
| MPD_030
| MPD_031
| MPD_032
| MPD_033
| MPD_034
| MPD_035
| MPD_036
| MPD_037
| MPD_038
| MPD_039
| MPD_040
| MPD_041
| MPD_042
| MPD_043
| MPD_044
| MPD_045
| MPD_046
| MPD_047
| MPD_048
| MPD_049
| MPD_050
| MPD_051
| MPD_052

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
| Charge_18USC371
| Charge_18USC372
| Charge_18USC1001
| Charge_18USC1519
| Charge_18USC241

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

module PreJan6Conspiracy :
 sig
  val dec_2020_06 : minute

  val dec_2020_07 : minute

  val dec_2020_09 : minute

  val dec_2020_19 : minute

  val dec_2020_29 : minute

  val jan_2021_02 : minute

  val jan_2021_04 : minute

  val jan_2021_05 : minute

  type coq_ConspiracyActor =
  | CA_Trump
  | CA_Eastman
  | CA_Chesebro
  | CA_Giuliani
  | CA_Meadows
  | CA_Clark
  | CA_Ellis
  | CA_Powell
  | CA_Tarrio
  | CA_Rhodes
  | CA_Meggs
  | CA_Biggs
  | CA_Nordean

  type coq_ConspiracyMemo =
  | CheseberoMemo_Dec6
  | CheseberoMemo_Dec9
  | EastmanMemo_TwoPage
  | EastmanMemo_SixPage

  type coq_LegalMemo = { memo_type : coq_ConspiracyMemo;
                         memo_author : coq_ConspiracyActor;
                         memo_date : minute; memo_pages : int;
                         memo_subject : string; memo_source : string }

  val chesebro_dec6_memo : coq_LegalMemo

  val chesebro_dec9_memo : coq_LegalMemo

  val eastman_twopage_memo : coq_LegalMemo

  val eastman_sixpage_memo : coq_LegalMemo

  val conspiracy_memos : coq_LegalMemo list

  type coq_PhoneCall = { call_date : minute; call_duration_minutes : 
                         int; call_caller : coq_ConspiracyActor;
                         call_participants : string list;
                         call_key_quote : string; call_source : string }

  val georgia_call : coq_PhoneCall

  type coq_MilitiaStructure =
  | ProudBoys_MOSD
  | OathKeepers_QRF
  | OathKeepers_StackOne
  | OathKeepers_StackTwo

  type coq_MilitiaPlanning = { mp_group : extremistGroup;
                               mp_structure : coq_MilitiaStructure;
                               mp_creation_date : minute;
                               mp_leader : convictedDefendant;
                               mp_comm_platform : string;
                               mp_purpose : string; mp_source : string }

  val proud_boys_mosd : coq_MilitiaPlanning

  val oath_keepers_qrf : coq_MilitiaPlanning

  val militia_planning_evidence : coq_MilitiaPlanning list

  type coq_WeaponsCache = { wc_location : string; wc_address : string;
                            wc_distance_from_capitol_miles : int;
                            wc_group : extremistGroup;
                            wc_contents : string list;
                            wc_state_teams : string list; wc_source : 
                            string }

  val comfort_inn_cache : coq_WeaponsCache

  type coq_EncryptedMessage = { em_platform : string; em_channel : string;
                                em_sender : convictedDefendant;
                                em_date : minute; em_content : string;
                                em_source : string }

  val tarrio_storm_message : coq_EncryptedMessage

  val tarrio_after_message : coq_EncryptedMessage

  val meggs_qrf_message : coq_EncryptedMessage

  val watkins_weapons_message : coq_EncryptedMessage

  val conspiracy_messages : coq_EncryptedMessage list
 end

module Communications :
 sig
  type coq_CommType =
  | PhoneCallComm
  | TextMessage
  | EncryptedChat
  | Email
  | InPersonMeeting
  | Tweet
  | PublicStatement

  type coq_Communication = { comm_type : coq_CommType;
                             comm_timestamp : minute; comm_from : string;
                             comm_to : string list; comm_summary : string;
                             comm_docket : string }

  val meadows_to_miller_dec6 : coq_Communication

  val eastman_forwarded_chesebro_dec7 : coq_Communication

  val trump_call_pence_jan5_1118 : coq_Communication

  val trump_call_pence_jan6_1111 : coq_Communication

  val mccarthy_call_trump_jan6 : coq_Communication

  val tuberville_call_trump_jan6 : coq_Communication

  val cipollone_warning_jan3 : coq_Communication

  val key_communications : coq_Communication list
 end

module LegalOutcomes :
 sig
  type coq_CaseStatus =
  | CaseActive
  | CaseDismissed
  | CaseConviction
  | CasePleaDeal
  | CaseAcquittal
  | CaseWithdrawn
  | CaseDisqualified

  type coq_ProsecutingEntity =
  | DOJ_MainJustice
  | DOJ_SpecialCounsel
  | FultonCountyDA
  | ArizonaAG
  | MichiganAG
  | NevadaAG
  | WisconsinDA
  | StateBarCalifornia
  | StateBarNewYork
  | StateBarColorado
  | StateBarGeorgia

  type coq_CriminalReferral = { ref_source : string; ref_date : string;
                                ref_target : string;
                                ref_charges : string list; ref_result : 
                                string }

  val house_referral_trump : coq_CriminalReferral

  val house_referral_eastman : coq_CriminalReferral

  val house_referral_meadows : coq_CriminalReferral

  val house_referral_giuliani : coq_CriminalReferral

  val house_referrals : coq_CriminalReferral list

  type coq_FederalCase = { fc_name : string; fc_docket : string;
                           fc_defendant : string;
                           fc_prosecutor : coq_ProsecutingEntity;
                           fc_charges : federalCharge list;
                           fc_indictment_date : string;
                           fc_status : coq_CaseStatus; fc_disposition : 
                           string }

  val usa_v_trump_dc : coq_FederalCase

  type coq_StateCase = { sc_name : string; sc_docket : string;
                         sc_defendant : string;
                         sc_prosecutor : coq_ProsecutingEntity;
                         sc_charges_count : int; sc_defendants_count : 
                         int; sc_indictment_date : string;
                         sc_status : coq_CaseStatus; sc_disposition : 
                         string }

  val georgia_v_trump : coq_StateCase

  type coq_DisbarmentProceeding = { disbar_attorney : string;
                                    disbar_bar : coq_ProsecutingEntity;
                                    disbar_filing_date : string;
                                    disbar_status : coq_CaseStatus;
                                    disbar_outcome : string }

  val eastman_disbarment : coq_DisbarmentProceeding

  val giuliani_disbarment_ny : coq_DisbarmentProceeding

  val giuliani_disbarment_dc : coq_DisbarmentProceeding

  val ellis_disbarment : coq_DisbarmentProceeding

  val disbarment_proceedings : coq_DisbarmentProceeding list

  type coq_SpecialCounselReport = { scr_counsel : string;
                                    scr_release_date : string;
                                    scr_pages : int; scr_conclusion : 
                                    string; scr_source : string }

  val smith_final_report : coq_SpecialCounselReport
 end

module CausalStructure :
 sig
  type coq_CausalRelation =
  | Enables
  | Causes
  | Precedes
  | Motivates
  | Facilitates
  | RespondsTo

  type coq_EventLink = { link_from : eventType; link_to : eventType;
                         link_relation : coq_CausalRelation;
                         link_evidence : string }

  val speech_enables_march : coq_EventLink

  val barricade_enables_building_breach : coq_EventLink

  val building_breach_causes_recess : coq_EventLink

  val breach_causes_evacuation : coq_EventLink

  val tweet_motivates_violence : coq_EventLink

  val guard_denial_facilitates_breach : coq_EventLink

  val causal_links : coq_EventLink list

  type coq_CrowdEstimate = { ce_location : dCLocation; ce_time : minute;
                             ce_count_low : int; ce_count_high : int;
                             ce_source : string }

  val ellipse_rally_crowd : coq_CrowdEstimate

  val capitol_grounds_crowd : coq_CrowdEstimate

  val inside_building_crowd : coq_CrowdEstimate

  val crowd_estimates : coq_CrowdEstimate list

  type coq_WeaponEvidence = { we_type : string; we_count : int;
                              we_location : dCLocation; we_seized : bool;
                              we_source : string }

  val flagpoles : coq_WeaponEvidence

  val bear_spray : coq_WeaponEvidence

  val tasers : coq_WeaponEvidence

  val baseball_bats : coq_WeaponEvidence

  val knives : coq_WeaponEvidence

  val firearms_seized : coq_WeaponEvidence

  val weapons_evidence : coq_WeaponEvidence list
 end

module Sources :
 sig
  type coq_SourceType =
  | CongressionalRecord
  | CourtFiling
  | CourtTranscript
  | JudicialOpinion
  | OfficialReport
  | VideoEvidence
  | AudioRecording
  | WitnessTestimony
  | DocumentaryEvidence
  | NewsReport

  type coq_Source = { src_type : coq_SourceType; src_title : string;
                      src_identifier : string; src_date : string;
                      src_url : string option }

  val src_house_final_report : coq_Source

  val src_rhodes_indictment : coq_Source

  val src_tarrio_indictment : coq_Source

  val src_trump_indictment : coq_Source

  val src_doj_ig_guard : coq_Source

  val src_dc_ocme_babbitt : coq_Source

  val src_dc_ocme_sicknick : coq_Source

  val src_cspan_rally : coq_Source

  val src_wapo_georgia_audio : coq_Source

  val src_smith_final_report : coq_Source

  val primary_sources : coq_Source list
 end

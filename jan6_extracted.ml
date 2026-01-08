
type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b



(** val add : int -> int -> int **)

let rec add n m =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> m)
    (fun p -> (fun n -> n + 1) (add p m))
    n

(** val mul : int -> int -> int **)

let rec mul n m =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> 0)
    (fun p -> add m (mul p m))
    n

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

type string =
| EmptyString
| String of ascii * string

type minute = int

(** val time_of_day : int -> int -> minute **)

let time_of_day h m =
  add
    (mul h ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) m

(** val next_day_offset : minute **)

let next_day_offset =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))))))))))) 0

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

(** val t_0653 : minute **)

let t_0653 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val t_1100 : minute **)

let t_1100 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))) 0

(** val t_1200 : minute **)

let t_1200 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))) 0

(** val t_1253 : minute **)

let t_1253 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val t_1300 : minute **)

let t_1300 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) 0

(** val t_1305 : minute **)

let t_1305 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))

(** val t_1310 : minute **)

let t_1310 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0))))))))))

(** val t_1311 : minute **)

let t_1311 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))

(** val t_1349 : minute **)

let t_1349 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))))

(** val t_1350 : minute **)

let t_1350 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0))))))))))))))))))))))))))))))))))))))))))))))))))

(** val t_1358 : minute **)

let t_1358 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val t_1400 : minute **)

let t_1400 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))) 0

(** val t_1411 : minute **)

let t_1411 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))

(** val t_1412 : minute **)

let t_1412 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))

(** val t_1413 : minute **)

let t_1413 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))

(** val t_1414 : minute **)

let t_1414 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))

(** val t_1415 : minute **)

let t_1415 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))))

(** val t_1418 : minute **)

let t_1418 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))

(** val t_1424 : minute **)

let t_1424 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))

(** val t_1425 : minute **)

let t_1425 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))))))))))))

(** val t_1426 : minute **)

let t_1426 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))))))))))

(** val t_1430 : minute **)

let t_1430 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))))))))))))))

(** val t_1444 : minute **)

let t_1444 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))

(** val t_1515 : minute **)

let t_1515 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))

(** val t_1521 : minute **)

let t_1521 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))))))))))

(** val t_1533 : minute **)

let t_1533 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))))))))))))))))))))))

(** val t_1617 : minute **)

let t_1617 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))))))

(** val t_1632 : minute **)

let t_1632 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))))))))))))))))

(** val t_1636 : minute **)

let t_1636 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))))))))))))))))))))

(** val t_1720 : minute **)

let t_1720 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))

(** val t_1800 : minute **)

let t_1800 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))
    0

(** val t_2000 : minute **)

let t_2000 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))))))))))))) 0

(** val t_2100 : minute **)

let t_2100 =
  time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))))))))) 0

(** val t_next_0006 : minute **)

let t_next_0006 =
  add next_day_offset
    (time_of_day 0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))

(** val t_next_0344 : minute **)

let t_next_0344 =
  add next_day_offset
    (time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0)))))))))))))))))))))))))))))))))))))))))))))

(** val ev_pipebomb_placed : event **)

let ev_pipebomb_placed =
  { event_time = t_0653; event_type = PipeBombDiscovered; event_location =
    RNCHeadquarters; event_actor = Some; event_source = (String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, false, false, true, false, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))) }

(** val ev_rally_begins : event **)

let ev_rally_begins =
  { event_time = t_1100; event_type = RallyBegins; event_location =
    WhiteHouseEllipse; event_actor = Some; event_source = (String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (true, false, true, true, false, true, false, false)), (String ((Ascii
    (true, true, false, false, true, false, true, false)), (String ((Ascii
    (false, false, false, false, true, false, true, false)), (String ((Ascii
    (true, false, false, false, false, false, true, false)), (String ((Ascii
    (false, true, true, true, false, false, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, true, true, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (true, true, true, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_trump_speech_begins : event **)

let ev_trump_speech_begins =
  { event_time = t_1200; event_type = SpeechBegins; event_location =
    WhiteHouseEllipse; event_actor = (None (ExecutiveActor DonaldTrump));
    event_source = (String ((Ascii (true, true, true, false, true, false,
    true, false)), (String ((Ascii (false, false, false, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, false, false, true, false, false,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, true, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))))))))) }

(** val ev_joint_session_convenes : event **)

let ev_joint_session_convenes =
  { event_time = t_1300; event_type = JointSessionConvenes; event_location =
    (CapitolBuilding HouseChamber); event_actor = (None (ElectedActor
    MikePence)); event_source = (String ((Ascii (true, true, false, false,
    false, false, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_pipebomb_rnc_found : event **)

let ev_pipebomb_rnc_found =
  { event_time = t_1305; event_type = PipeBombDiscovered; event_location =
    RNCHeadquarters; event_actor = Some; event_source = (String ((Ascii
    (false, true, true, false, false, false, true, false)), (String ((Ascii
    (false, true, false, false, false, false, true, false)), (String ((Ascii
    (true, false, false, true, false, false, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))) }

(** val ev_speech_ends : event **)

let ev_speech_ends =
  { event_time = t_1310; event_type = SpeechEnds; event_location =
    WhiteHouseEllipse; event_actor = (None (ExecutiveActor DonaldTrump));
    event_source = (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (true, false, true, true, false, true,
    false, false)), (String ((Ascii (true, true, false, false, true, false,
    true, false)), (String ((Ascii (false, false, false, false, true, false,
    true, false)), (String ((Ascii (true, false, false, false, false, false,
    true, false)), (String ((Ascii (false, true, true, true, false, false,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, true, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))))))))))))))) }

(** val ev_march_instruction : event **)

let ev_march_instruction =
  { event_time = t_1311; event_type = MarchInstruction; event_location =
    WhiteHouseEllipse; event_actor = (None (ExecutiveActor DonaldTrump));
    event_source = (String ((Ascii (true, true, false, false, true, false,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))))) }

(** val ev_guard_requested_sund : event **)

let ev_guard_requested_sund =
  { event_time = t_1349; event_type = GuardRequested; event_location =
    Pentagon; event_actor = (None (SecurityActor StevenSund)); event_source =
    (String ((Ascii (false, false, false, true, false, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, false, true, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (true, true, false, false, true, false, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, true, false, true, true, false)),
    (String ((Ascii (true, false, true, true, false, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))) }

(** val ev_riot_declared : event **)

let ev_riot_declared =
  { event_time = t_1350; event_type = RiotDeclared; event_location =
    CapitolGrounds; event_actor = Some; event_source = (String ((Ascii (true,
    false, true, true, false, false, true, false)), (String ((Ascii (false,
    false, false, false, true, false, true, false)), (String ((Ascii (false,
    false, true, false, false, false, true, false)), (String ((Ascii (false,
    false, false, false, false, true, false, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (true,
    false, false, false, false, true, true, false)), (String ((Ascii (false,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, false, false, true, false, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (false,
    true, false, false, true, true, true, false)), (String ((Ascii (true,
    false, false, false, false, true, true, false)), (String ((Ascii (false,
    true, true, false, false, true, true, false)), (String ((Ascii (false,
    true, true, false, false, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (true,
    true, false, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))) }

(** val ev_first_barricade : event **)

let ev_first_barricade =
  { event_time = t_1253; event_type = BarricadeBreach; event_location =
    CapitolGrounds; event_actor = Some; event_source = (String ((Ascii
    (false, true, true, false, true, false, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, true, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_north_barricade : event **)

let ev_north_barricade =
  { event_time = t_1358; event_type = BarricadeBreach; event_location =
    (CapitolBuilding NorthBarricade); event_actor = Some; event_source =
    (String ((Ascii (true, false, true, false, true, false, true, false)),
    (String ((Ascii (true, true, false, false, true, false, true, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (false, false, false, false, true, false, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))) }

(** val ev_police_line_west : event **)

let ev_police_line_west =
  { event_time = t_1400; event_type = PoliceLineCollapse; event_location =
    (CapitolBuilding WestFront); event_actor = Some; event_source = (String
    ((Ascii (false, true, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_window_breach : event **)

let ev_window_breach =
  { event_time = t_1411; event_type = WindowBreach; event_location =
    (CapitolBuilding WestFront); event_actor = (None (ConvictedActor
    DominicPezzola)); event_source = (String ((Ascii (false, false, true,
    false, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, false, true, false)), (String ((Ascii (false, true, false,
    true, false, false, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (true, true, false,
    false, false, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (true, false, true,
    false, true, true, true, false)), (String ((Ascii (false, true, false,
    false, true, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (false, true, true,
    false, false, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, false, true,
    true, false, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, true, true,
    false, false, true, true, false)), (String ((Ascii (true, true, false,
    false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))) }

(** val ev_building_breach_west : event **)

let ev_building_breach_west =
  { event_time = t_1412; event_type = BuildingBreach; event_location =
    (CapitolBuilding WestFront); event_actor = Some; event_source = (String
    ((Ascii (false, true, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_senate_recesses : event **)

let ev_senate_recesses =
  { event_time = t_1413; event_type = JointSessionRecesses; event_location =
    (CapitolBuilding SenateChamber); event_actor = Some; event_source =
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, true, true, false, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (false, true, false, false, true, false, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_pence_evacuated : event **)

let ev_pence_evacuated =
  { event_time = t_1414; event_type = Evacuation; event_location =
    (CapitolBuilding SenateHideaway); event_actor = (None (ElectedActor
    MikePence)); event_source = (String ((Ascii (true, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, true, false,
    true, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), EmptyString)))))))))))))))))))))))))))) }

(** val ev_goodman_redirects_romney : event **)

let ev_goodman_redirects_romney =
  { event_time = t_1414; event_type = Evacuation; event_location =
    (CapitolBuilding SenateWing); event_actor = (None (OfficerActor
    EugeneGoodman)); event_source = (String ((Ascii (false, true, true,
    false, true, false, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, false, true,
    false, false, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (false, true, true,
    false, true, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, false, true,
    false, false, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, true, false,
    false, false, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_rioters_senate_floor : event **)

let ev_rioters_senate_floor =
  { event_time = t_1415; event_type = RoomBreach; event_location =
    (CapitolBuilding SenateChamber); event_actor = Some; event_source =
    (String ((Ascii (false, true, true, false, true, false, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (false, true, true, false, true, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_house_recesses : event **)

let ev_house_recesses =
  { event_time = t_1418; event_type = JointSessionRecesses; event_location =
    (CapitolBuilding HouseChamber); event_actor = Some; event_source =
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, true, true, false, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (false, true, false, false, true, false, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_pence_tweet : event **)

let ev_pence_tweet =
  { event_time = t_1424; event_type = TweetPosted; event_location = Pentagon;
    event_actor = (None (ExecutiveActor DonaldTrump)); event_source = (String
    ((Ascii (false, false, true, false, true, false, true, false)), (String
    ((Ascii (true, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (false, false, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, false, false, false, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (false, false, false, true, false, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))) }

(** val ev_pence_40ft : event **)

let ev_pence_40ft =
  { event_time = t_1425; event_type = Evacuation; event_location =
    (CapitolBuilding CryptLevel); event_actor = (None (ElectedActor
    MikePence)); event_source = (String ((Ascii (true, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, true, false,
    true, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (true, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, true, true, false,
    true, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))) }

(** val ev_crypt_breach : event **)

let ev_crypt_breach =
  { event_time = t_1425; event_type = RoomBreach; event_location =
    (CapitolBuilding CryptLevel); event_actor = Some; event_source = (String
    ((Ascii (false, true, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_mccarthy_call : event **)

let ev_mccarthy_call =
  { event_time = t_1426; event_type = PhoneCall; event_location =
    (CapitolBuilding HouseWing); event_actor = (None (ElectedActor
    KevinMcCarthy)); event_source = (String ((Ascii (false, false, true,
    false, true, false, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (true, true, false,
    false, true, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (true, false, true,
    true, false, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (true, false, false,
    true, true, true, true, false)), EmptyString)))))))))))))))))) }

(** val ev_rotunda_breach : event **)

let ev_rotunda_breach =
  { event_time = t_1430; event_type = RoomBreach; event_location =
    (CapitolBuilding Rotunda); event_actor = Some; event_source = (String
    ((Ascii (false, true, true, false, true, false, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, false, true, true, true, false)), (String
    ((Ascii (true, false, false, true, false, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (false, true, true, true, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_babbitt_shot : event **)

let ev_babbitt_shot =
  { event_time = t_1444; event_type = CivilianShot; event_location =
    (CapitolBuilding SpeakerLobby); event_actor = (None (DeceasedCivilian
    AshliBabbitt)); event_source = (String ((Ascii (false, false, true,
    false, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, false, true, false)), (String ((Ascii (false, true, false,
    true, false, false, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    false, true, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (true, true, false,
    false, true, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (true, true, true,
    false, false, true, true, false)), (String ((Ascii (true, false, false,
    false, false, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), (String ((Ascii (true, false, false,
    true, false, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, true,
    true, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))) }

(** val ev_tunnel_crush_hodges : event **)

let ev_tunnel_crush_hodges =
  { event_time = t_1515; event_type = OfficerCrushed; event_location =
    (CapitolBuilding LowerWestTerraceTunnel); event_actor = (None
    (OfficerActor DanielHodges)); event_source = (String ((Ascii (false,
    true, true, false, true, false, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    false, false, false, false, true, false, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (false,
    true, true, false, true, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (false,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (true,
    true, false, false, false, true, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_tunnel_assault_fanone : event **)

let ev_tunnel_assault_fanone =
  { event_time = t_1521; event_type = OfficerAssault; event_location =
    (CapitolBuilding LowerWestTerraceTunnel); event_actor = (None
    (OfficerActor MichaelFanone)); event_source = (String ((Ascii (false,
    false, true, false, true, false, true, false)), (String ((Ascii (true,
    false, true, false, false, true, true, false)), (String ((Ascii (true,
    true, false, false, true, true, true, false)), (String ((Ascii (false,
    false, true, false, true, true, true, false)), (String ((Ascii (true,
    false, false, true, false, true, true, false)), (String ((Ascii (true,
    false, true, true, false, true, true, false)), (String ((Ascii (true,
    true, true, true, false, true, true, false)), (String ((Ascii (false,
    true, true, true, false, true, true, false)), (String ((Ascii (true,
    false, false, true, true, true, true, false)),
    EmptyString)))))))))))))))))) }

(** val ev_pipebomb_rnc_neutralized : event **)

let ev_pipebomb_rnc_neutralized =
  { event_time = t_1533; event_type = PipeBombNeutralized; event_location =
    RNCHeadquarters; event_actor = Some; event_source = (String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, false, false, true, false, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))) }

(** val ev_guard_authorized : event **)

let ev_guard_authorized =
  { event_time = t_1632; event_type = GuardAuthorized; event_location =
    Pentagon; event_actor = (None (MilitaryActor ChristopherMiller));
    event_source = (String ((Ascii (false, false, true, false, false, false,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, false, false,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (true, false, false, true, false, false,
    true, false)), (String ((Ascii (true, true, true, false, false, false,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, false, false, false, true, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), EmptyString)))))))))))))))))))))))))) }

(** val ev_pipebomb_dnc_neutralized : event **)

let ev_pipebomb_dnc_neutralized =
  { event_time = t_1636; event_type = PipeBombNeutralized; event_location =
    DNCHeadquarters; event_actor = Some; event_source = (String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, false, false, true, false, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, true, false, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (false, false, true, false, true, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (true, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, false, true, true, false, true, true, false)), (String ((Ascii
    (true, false, false, true, false, true, true, false)), (String ((Ascii
    (false, true, true, true, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))))))))) }

(** val ev_trump_video : event **)

let ev_trump_video =
  { event_time = t_1617; event_type = VideoPosted; event_location =
    WhiteHouseEllipse; event_actor = (None (ExecutiveActor DonaldTrump));
    event_source = (String ((Ascii (false, false, true, false, true, false,
    true, false)), (String ((Ascii (true, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (false, false, false, true, false, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (false, true, true, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))) }

(** val ev_guard_arrives : event **)

let ev_guard_arrives =
  { event_time = t_1720; event_type = GuardArrives; event_location =
    DCNationalGuardArmory; event_actor = (None (MilitaryActor
    WilliamWalker)); event_source = (String ((Ascii (false, false, true,
    false, false, false, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, false, true,
    false, false, false, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (true, false, false,
    true, false, false, true, false)), (String ((Ascii (true, true, true,
    false, false, false, true, false)), (String ((Ascii (false, false, false,
    false, false, true, false, false)), (String ((Ascii (false, true, false,
    false, true, true, true, false)), (String ((Ascii (true, false, true,
    false, false, true, true, false)), (String ((Ascii (false, false, false,
    false, true, true, true, false)), (String ((Ascii (true, true, true,
    true, false, true, true, false)), (String ((Ascii (false, true, false,
    false, true, true, true, false)), (String ((Ascii (false, false, true,
    false, true, true, true, false)), EmptyString)))))))))))))))))))))))))) }

(** val ev_capitol_cleared : event **)

let ev_capitol_cleared =
  { event_time = t_1800; event_type = CapitolCleared; event_location =
    (CapitolBuilding Rotunda); event_actor = Some; event_source = (String
    ((Ascii (true, false, true, false, true, false, true, false)), (String
    ((Ascii (true, true, false, false, true, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (false, false, false, false, true, false, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (true, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, false, false, false, true, true, false)), (String
    ((Ascii (true, true, true, true, false, true, true, false)), (String
    ((Ascii (false, true, false, false, true, true, true, false)), (String
    ((Ascii (false, false, true, false, false, true, true, false)), (String
    ((Ascii (true, true, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))) }

(** val ev_curfew : event **)

let ev_curfew =
  { event_time = t_1800; event_type = CurfewDeclared; event_location =
    CapitolGrounds; event_actor = Some; event_source = (String ((Ascii
    (false, false, true, false, false, false, true, false)), (String ((Ascii
    (true, true, false, false, false, false, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (true, false, true, true, false, false, true, false)), (String ((Ascii
    (true, false, false, false, false, true, true, false)), (String ((Ascii
    (true, false, false, true, true, true, true, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (false, false, false, false, false, true, false, false)), (String ((Ascii
    (true, true, true, true, false, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)), (String ((Ascii
    (false, false, true, false, false, true, true, false)), (String ((Ascii
    (true, false, true, false, false, true, true, false)), (String ((Ascii
    (false, true, false, false, true, true, true, false)),
    EmptyString)))))))))))))))))))))))))))) }

(** val ev_session_resumes : event **)

let ev_session_resumes =
  { event_time = t_2000; event_type = JointSessionResumes; event_location =
    (CapitolBuilding HouseChamber); event_actor = (None (ElectedActor
    MikePence)); event_source = (String ((Ascii (true, true, false, false,
    false, false, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, true, true, false,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (true, true, false, false,
    true, true, true, false)), (String ((Ascii (true, false, false, true,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, true, true,
    false, true, true, false)), (String ((Ascii (true, false, false, false,
    false, true, true, false)), (String ((Ascii (false, false, true, true,
    false, true, true, false)), (String ((Ascii (false, false, false, false,
    false, true, false, false)), (String ((Ascii (false, true, false, false,
    true, false, true, false)), (String ((Ascii (true, false, true, false,
    false, true, true, false)), (String ((Ascii (true, true, false, false,
    false, true, true, false)), (String ((Ascii (true, true, true, true,
    false, true, true, false)), (String ((Ascii (false, true, false, false,
    true, true, true, false)), (String ((Ascii (false, false, true, false,
    false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_arizona_objection_fails : event **)

let ev_arizona_objection_fails =
  { event_time = t_2100; event_type = ObjectionVoteFails; event_location =
    (CapitolBuilding HouseChamber); event_actor = Some; event_source =
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, true, true, false, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, true, false, false, true, true, true, false)),
    (String ((Ascii (true, false, false, true, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, true, true, false, true, true, false)),
    (String ((Ascii (true, false, false, false, false, true, true, false)),
    (String ((Ascii (false, false, true, true, false, true, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (false, true, false, false, true, false, true, false)),
    (String ((Ascii (true, false, true, false, false, true, true, false)),
    (String ((Ascii (true, true, false, false, false, true, true, false)),
    (String ((Ascii (true, true, true, true, false, true, true, false)),
    (String ((Ascii (false, true, false, false, true, true, true, false)),
    (String ((Ascii (false, false, true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_pennsylvania_objection_fails : event **)

let ev_pennsylvania_objection_fails =
  { event_time = t_next_0006; event_type = ObjectionVoteFails;
    event_location = (CapitolBuilding HouseChamber); event_actor = Some;
    event_source = (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, true, true, false, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, true, false, false, true, true,
    true, false)), (String ((Ascii (true, false, false, true, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, true, true, false, true,
    true, false)), (String ((Ascii (true, false, false, false, false, true,
    true, false)), (String ((Ascii (false, false, true, true, false, true,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (false, true, false, false, true, false,
    true, false)), (String ((Ascii (true, false, true, false, false, true,
    true, false)), (String ((Ascii (true, true, false, false, false, true,
    true, false)), (String ((Ascii (true, true, true, true, false, true,
    true, false)), (String ((Ascii (false, true, false, false, true, true,
    true, false)), (String ((Ascii (false, false, true, false, false, true,
    true, false)), EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val ev_certification_complete : event **)

let ev_certification_complete =
  { event_time = t_next_0344; event_type = CertificationCompletes;
    event_location = (CapitolBuilding HouseChamber); event_actor = (None
    (ElectedActor MikePence)); event_source = (String ((Ascii (true, true,
    false, false, false, false, true, false)), (String ((Ascii (true, true,
    true, true, false, true, true, false)), (String ((Ascii (false, true,
    true, true, false, true, true, false)), (String ((Ascii (true, true,
    true, false, false, true, true, false)), (String ((Ascii (false, true,
    false, false, true, true, true, false)), (String ((Ascii (true, false,
    true, false, false, true, true, false)), (String ((Ascii (true, true,
    false, false, true, true, true, false)), (String ((Ascii (true, true,
    false, false, true, true, true, false)), (String ((Ascii (true, false,
    false, true, false, true, true, false)), (String ((Ascii (true, true,
    true, true, false, true, true, false)), (String ((Ascii (false, true,
    true, true, false, true, true, false)), (String ((Ascii (true, false,
    false, false, false, true, true, false)), (String ((Ascii (false, false,
    true, true, false, true, true, false)), (String ((Ascii (false, false,
    false, false, false, true, false, false)), (String ((Ascii (false, true,
    false, false, true, false, true, false)), (String ((Ascii (true, false,
    true, false, false, true, true, false)), (String ((Ascii (true, true,
    false, false, false, true, true, false)), (String ((Ascii (true, true,
    true, true, false, true, true, false)), (String ((Ascii (false, true,
    false, false, true, true, true, false)), (String ((Ascii (false, false,
    true, false, false, true, true, false)),
    EmptyString)))))))))))))))))))))))))))))))))))))))) }

(** val timeline : event list **)

let timeline =
  ev_pipebomb_placed::(ev_rally_begins::(ev_trump_speech_begins::(ev_first_barricade::(ev_joint_session_convenes::(ev_pipebomb_rnc_found::(ev_speech_ends::(ev_march_instruction::(ev_guard_requested_sund::(ev_riot_declared::(ev_north_barricade::(ev_police_line_west::(ev_window_breach::(ev_building_breach_west::(ev_senate_recesses::(ev_pence_evacuated::(ev_goodman_redirects_romney::(ev_rioters_senate_floor::(ev_house_recesses::(ev_pence_tweet::(ev_pence_40ft::(ev_crypt_breach::(ev_mccarthy_call::(ev_rotunda_breach::(ev_babbitt_shot::(ev_tunnel_crush_hodges::(ev_tunnel_assault_fanone::(ev_pipebomb_rnc_neutralized::(ev_trump_video::(ev_guard_authorized::(ev_pipebomb_dnc_neutralized::(ev_guard_arrives::(ev_capitol_cleared::(ev_curfew::(ev_session_resumes::(ev_arizona_objection_fails::(ev_pennsylvania_objection_fails::(ev_certification_complete::[])))))))))))))))))))))))))))))))))))))

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

(** val conv_rhodes : conviction **)

let conv_rhodes =
  { conv_defendant = StewartRhodes; conv_group = OathKeepers; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512c2::(Charge_18USC1519::[])));
    conv_verdict_date = (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (true, false, false, false, true,
    true, false, false)), (String ((Ascii (true, false, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (true, false, false, true, true,
    true, false, false)), EmptyString))))))))))))))))))));
    conv_sentence_months = ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::(LeadershipEnhancement::[]));
    conv_trial_outcome = JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_meggs : conviction **)

let conv_meggs =
  { conv_defendant = KellyMeggs; conv_group = OathKeepers; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512k::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, true, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::[]); conv_trial_outcome =
    JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_watkins : conviction **)

let conv_watkins =
  { conv_defendant = JessicaWatkins; conv_group = OathKeepers; conv_charges =
    (Charge_18USC1512c2::(Charge_18USC231::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, true, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = []; conv_trial_outcome = JuryVerdict;
    conv_status_jan2025 = Pardoned }

(** val conv_harrelson : conviction **)

let conv_harrelson =
  { conv_defendant = KennethHarrelson; conv_group = OathKeepers;
    conv_charges = (Charge_18USC1512c2::[]); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, false, true, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))); conv_enhancements =
    []; conv_trial_outcome = JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_tarrio : conviction **)

let conv_tarrio =
  { conv_defendant = EnriqueTarrio; conv_group = ProudBoys; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512k::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, false, true, false, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::(LeadershipEnhancement::[]));
    conv_trial_outcome = JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_nordean : conviction **)

let conv_nordean =
  { conv_defendant = EthanNordean; conv_group = ProudBoys; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512k::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, false, true, false, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::[]); conv_trial_outcome =
    JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_biggs : conviction **)

let conv_biggs =
  { conv_defendant = JosephBiggs; conv_group = ProudBoys; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512k::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, false, true, false, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::[]); conv_trial_outcome =
    JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_rehl : conviction **)

let conv_rehl =
  { conv_defendant = ZacharyRehl; conv_group = ProudBoys; conv_charges =
    (Charge_18USC2384::(Charge_18USC1512k::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, false, true, false, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = (TerrorismEnhancement::[]); conv_trial_outcome =
    JuryVerdict; conv_status_jan2025 = Pardoned }

(** val conv_pezzola : conviction **)

let conv_pezzola =
  { conv_defendant = DominicPezzola; conv_group = ProudBoys; conv_charges =
    (Charge_18USC1512c2::(Charge_18USC111::(Charge_18USC231::(Charge_18USC1361::[]))));
    conv_verdict_date = (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (true, true, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (false, false, true, false, true,
    true, false, false)), EmptyString))))))))))))))))))));
    conv_sentence_months = ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = []; conv_trial_outcome = JuryVerdict;
    conv_status_jan2025 = Pardoned }

(** val conv_chansley : conviction **)

let conv_chansley =
  { conv_defendant = JacobChansley; conv_group = QAnon; conv_charges =
    (Charge_18USC1512c2::[]); conv_verdict_date = (String ((Ascii (false,
    true, false, false, true, true, false, false)), (String ((Ascii (false,
    false, false, false, true, true, false, false)), (String ((Ascii (false,
    true, false, false, true, true, false, false)), (String ((Ascii (true,
    false, false, false, true, true, false, false)), (String ((Ascii (true,
    false, true, true, false, true, false, false)), (String ((Ascii (false,
    false, false, false, true, true, false, false)), (String ((Ascii (true,
    false, false, true, true, true, false, false)), (String ((Ascii (true,
    false, true, true, false, true, false, false)), (String ((Ascii (false,
    false, false, false, true, true, false, false)), (String ((Ascii (true,
    true, false, false, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))); conv_enhancements = [];
    conv_trial_outcome = GuiltyPlea; conv_status_jan2025 = Pardoned }

(** val conv_barnett : conviction **)

let conv_barnett =
  { conv_defendant = RichardBarnett; conv_group = NoGroupAffiliation;
    conv_charges = (Charge_18USC1512c2::(Charge_18USC1752::[]));
    conv_verdict_date = (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (true, true, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (false, false, false, false, true,
    true, false, false)), (String ((Ascii (true, false, false, false, true,
    true, false, false)), (String ((Ascii (true, false, true, true, false,
    true, false, false)), (String ((Ascii (false, true, false, false, true,
    true, false, false)), (String ((Ascii (true, true, false, false, true,
    true, false, false)), EmptyString))))))))))))))))))));
    conv_sentence_months = ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = []; conv_trial_outcome = JuryVerdict;
    conv_status_jan2025 = Pardoned }

(** val conv_reffitt : conviction **)

let conv_reffitt =
  { conv_defendant = GuyReffitt; conv_group = ThreePercenters; conv_charges =
    (Charge_18USC1512c2::(Charge_18USC111::[])); conv_verdict_date = (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (false, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (true, true, false, false, true, true, false, false)), (String
    ((Ascii (true, false, true, true, false, true, false, false)), (String
    ((Ascii (false, false, false, false, true, true, false, false)), (String
    ((Ascii (false, false, false, true, true, true, false, false)),
    EmptyString)))))))))))))))))))); conv_sentence_months = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    conv_enhancements = []; conv_trial_outcome = JuryVerdict;
    conv_status_jan2025 = Pardoned }

(** val seditious_convictions : conviction list **)

let seditious_convictions =
  conv_rhodes::(conv_meggs::(conv_tarrio::(conv_nordean::(conv_biggs::(conv_rehl::[])))))

(** val all_convictions : conviction list **)

let all_convictions =
  conv_rhodes::(conv_meggs::(conv_watkins::(conv_harrelson::(conv_tarrio::(conv_nordean::(conv_biggs::(conv_rehl::(conv_pezzola::(conv_chansley::(conv_barnett::(conv_reffitt::[])))))))))))

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

(** val death_babbitt : decedent **)

let death_babbitt =
  { dec_name = (Inl AshliBabbitt); dec_category = Rioter; dec_location =
    (CapitolBuilding SpeakerLobby); dec_time = (None t_1444); dec_cause =
    Gunshot; dec_manner = Homicide; dec_days_after_jan6 = 0; dec_source =
    (String ((Ascii (false, false, true, false, false, false, true, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (true, true, true, true, false, false, true, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, false, true, true, false, false, true, false)),
    (String ((Ascii (true, false, true, false, false, false, true, false)),
    EmptyString)))))))))))))) }

(** val death_boyland : decedent **)

let death_boyland =
  { dec_name = (Inl RosanneBoyland); dec_category = Rioter; dec_location =
    (CapitolBuilding LowerWestTerrace); dec_time = Some; dec_cause =
    AcuteAmphetamineIntoxication; dec_manner = Accident;
    dec_days_after_jan6 = 0; dec_source = (String ((Ascii (false, false,
    true, false, false, false, true, false)), (String ((Ascii (true, true,
    false, false, false, false, true, false)), (String ((Ascii (false, false,
    false, false, false, true, false, false)), (String ((Ascii (true, true,
    true, true, false, false, true, false)), (String ((Ascii (true, true,
    false, false, false, false, true, false)), (String ((Ascii (true, false,
    true, true, false, false, true, false)), (String ((Ascii (true, false,
    true, false, false, false, true, false)), EmptyString)))))))))))))) }

(** val death_greeson : decedent **)

let death_greeson =
  { dec_name = (Inl KevinGreeson); dec_category = Rioter; dec_location =
    (CapitolBuilding WestFront); dec_time = Some; dec_cause = HeartAttackCOD;
    dec_manner = Natural; dec_days_after_jan6 = 0; dec_source = (String
    ((Ascii (false, false, true, false, false, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, true, true, true, false, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (true, false, true, true, false, false, true, false)), (String
    ((Ascii (true, false, true, false, false, false, true, false)),
    EmptyString)))))))))))))) }

(** val death_phillips : decedent **)

let death_phillips =
  { dec_name = (Inl BenjaminPhillips); dec_category = Rioter; dec_location =
    (CapitolBuilding WestFront); dec_time = Some; dec_cause = StrokeCOD;
    dec_manner = Natural; dec_days_after_jan6 = 0; dec_source = (String
    ((Ascii (false, false, true, false, false, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (false, false, false, false, false, true, false, false)), (String
    ((Ascii (true, true, true, true, false, false, true, false)), (String
    ((Ascii (true, true, false, false, false, false, true, false)), (String
    ((Ascii (true, false, true, true, false, false, true, false)), (String
    ((Ascii (true, false, true, false, false, false, true, false)),
    EmptyString)))))))))))))) }

(** val death_sicknick : decedent **)

let death_sicknick =
  { dec_name = (Inr BrianSicknick); dec_category = (LawEnforcement USCP);
    dec_location = (CapitolBuilding WestFront); dec_time = Some; dec_cause =
    StrokeCOD; dec_manner = Natural; dec_days_after_jan6 = ((fun n -> n + 1)
    0); dec_source = (String ((Ascii (false, false, true, false, false,
    false, true, false)), (String ((Ascii (true, true, false, false, false,
    false, true, false)), (String ((Ascii (false, false, false, false, false,
    true, false, false)), (String ((Ascii (true, true, true, true, false,
    false, true, false)), (String ((Ascii (true, true, false, false, false,
    false, true, false)), (String ((Ascii (true, false, true, true, false,
    false, true, false)), (String ((Ascii (true, false, true, false, false,
    false, true, false)), EmptyString)))))))))))))) }

(** val death_liebengood : decedent **)

let death_liebengood =
  { dec_name = (Inr HowardLiebengood); dec_category = (LawEnforcement USCP);
    dec_location = (CapitolBuilding WestFront); dec_time = Some; dec_cause =
    Suicide; dec_manner = SuicideClass; dec_days_after_jan6 =
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))); dec_source =
    (String ((Ascii (false, false, true, false, false, false, true, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (false, false, false, false, false, true, false, false)),
    (String ((Ascii (true, true, true, true, false, false, true, false)),
    (String ((Ascii (true, true, false, false, false, false, true, false)),
    (String ((Ascii (true, false, true, true, false, false, true, false)),
    (String ((Ascii (true, false, true, false, false, false, true, false)),
    EmptyString)))))))))))))) }

(** val death_smith : decedent **)

let death_smith =
  { dec_name = (Inr JeffreySmith); dec_category = (LawEnforcement MPD);
    dec_location = (CapitolBuilding WestFront); dec_time = Some; dec_cause =
    Suicide; dec_manner = SuicideClass; dec_days_after_jan6 =
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0))))))))); dec_source = (String ((Ascii (false, false,
    true, false, false, false, true, false)), (String ((Ascii (true, true,
    false, false, false, false, true, false)), (String ((Ascii (false, false,
    false, false, false, true, false, false)), (String ((Ascii (true, true,
    true, true, false, false, true, false)), (String ((Ascii (true, true,
    false, false, false, false, true, false)), (String ((Ascii (true, false,
    true, true, false, false, true, false)), (String ((Ascii (true, false,
    true, false, false, false, true, false)), EmptyString)))))))))))))) }

(** val death_hashida : decedent **)

let death_hashida =
  { dec_name = (Inr GuntherHashida); dec_category = (LawEnforcement MPD);
    dec_location = (CapitolBuilding WestFront); dec_time = Some; dec_cause =
    Suicide; dec_manner = SuicideClass; dec_days_after_jan6 =
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1)
    0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    dec_source = (String ((Ascii (false, false, true, false, false, false,
    true, false)), (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (true, true, true, true, false, false,
    true, false)), (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (true, false, true, true, false, false,
    true, false)), (String ((Ascii (true, false, true, false, false, false,
    true, false)), EmptyString)))))))))))))) }

(** val death_defreytag : decedent **)

let death_defreytag =
  { dec_name = (Inr KyleDeFretag); dec_category = (LawEnforcement MPD);
    dec_location = (CapitolBuilding WestFront); dec_time = Some; dec_cause =
    Suicide; dec_manner = SuicideClass; dec_days_after_jan6 =
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1)
    0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    dec_source = (String ((Ascii (false, false, true, false, false, false,
    true, false)), (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (false, false, false, false, false, true,
    false, false)), (String ((Ascii (true, true, true, true, false, false,
    true, false)), (String ((Ascii (true, true, false, false, false, false,
    true, false)), (String ((Ascii (true, false, true, true, false, false,
    true, false)), (String ((Ascii (true, false, true, false, false, false,
    true, false)), EmptyString)))))))))))))) }

(** val jan6_deaths : decedent list **)

let jan6_deaths =
  death_babbitt::(death_boyland::(death_greeson::(death_phillips::(death_sicknick::[]))))

(** val officer_suicides : decedent list **)

let officer_suicides =
  death_liebengood::(death_smith::(death_hashida::(death_defreytag::[])))

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

(** val ev_arizona : stateElectoralVotes **)

let ev_arizona =
  { sev_state = Arizona; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))); sev_winner = JoeBiden;
    sev_certified = true; sev_objection_filed = true;
    sev_objection_sustained = false; sev_fake_electors_submitted = true }

(** val ev_georgia : stateElectoralVotes **)

let ev_georgia =
  { sev_state = Georgia; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))));
    sev_winner = JoeBiden; sev_certified = true; sev_objection_filed = false;
    sev_objection_sustained = false; sev_fake_electors_submitted = true }

(** val ev_michigan : stateElectoralVotes **)

let ev_michigan =
  { sev_state = Michigan; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))))))));
    sev_winner = JoeBiden; sev_certified = true; sev_objection_filed = false;
    sev_objection_sustained = false; sev_fake_electors_submitted = true }

(** val ev_nevada : stateElectoralVotes **)

let ev_nevada =
  { sev_state = Nevada; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))); sev_winner = JoeBiden; sev_certified = true;
    sev_objection_filed = false; sev_objection_sustained = false;
    sev_fake_electors_submitted = true }

(** val ev_newmexico : stateElectoralVotes **)

let ev_newmexico =
  { sev_state = NewMexico; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0))))); sev_winner = JoeBiden; sev_certified = true;
    sev_objection_filed = false; sev_objection_sustained = false;
    sev_fake_electors_submitted = true }

(** val ev_pennsylvania : stateElectoralVotes **)

let ev_pennsylvania =
  { sev_state = Pennsylvania; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    0)))))))))))))))))))); sev_winner = JoeBiden; sev_certified = true;
    sev_objection_filed = true; sev_objection_sustained = false;
    sev_fake_electors_submitted = true }

(** val ev_wisconsin : stateElectoralVotes **)

let ev_wisconsin =
  { sev_state = Wisconsin; sev_electoral_votes = ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
    ((fun n -> n + 1) 0)))))))))); sev_winner = JoeBiden; sev_certified =
    true; sev_objection_filed = false; sev_objection_sustained = false;
    sev_fake_electors_submitted = true }

(** val contested_states : stateElectoralVotes list **)

let contested_states =
  ev_arizona::(ev_georgia::(ev_michigan::(ev_nevada::(ev_newmexico::(ev_pennsylvania::(ev_wisconsin::[]))))))

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

(** val jan6_certification : electoralCertification **)

let jan6_certification =
  { cert_presiding_officer = MikePence; cert_session_start = t_1300;
    cert_interruption_time = (None t_1413); cert_resumption_time = (None
    t_2000); cert_completion_time = (None t_next_0344); cert_final_state =
    Certified }

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

(** val jan6_guard_request : guardRequest **)

let jan6_guard_request =
  { gr_requester = StevenSund; gr_request_time = t_1349;
    gr_initial_response = Pending; gr_authorization_time = (None t_1632);
    gr_authorizing_official = (None ChristopherMiller); gr_deployment_time =
    (None t_1720) }

type pipeBomb = { pb_location : dCLocation;
                  pb_placement_time : minute option;
                  pb_discovery_time : minute;
                  pb_neutralization_time : minute; pb_bomber_identified : 
                  bool }

(** val rnc_pipebomb : pipeBomb **)

let rnc_pipebomb =
  { pb_location = RNCHeadquarters; pb_placement_time = (None t_0653);
    pb_discovery_time = t_1305; pb_neutralization_time = t_1533;
    pb_bomber_identified = false }

(** val dnc_pipebomb : pipeBomb **)

let dnc_pipebomb =
  { pb_location = DNCHeadquarters; pb_placement_time = (None t_0653);
    pb_discovery_time = t_1305; pb_neutralization_time = t_1636;
    pb_bomber_identified = false }

(** val pipebombs : pipeBomb list **)

let pipebombs =
  rnc_pipebomb::(dnc_pipebomb::[])

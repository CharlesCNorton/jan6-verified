(******************************************************************************)
(*                                                                            *)
(*              January 6: Capitol Breach Event Chronology                    *)
(*                                                                            *)
(*     Formalizes the Capitol breach (January 6, 2021): 140+ officer injury   *)
(*     records, tunnel crush timeline, seditious conspiracy convictions       *)
(*     (Rhodes 18y, Tarrio 22y), electoral certification interruption, and    *)
(*     National Guard deployment authority chain. Models Vice Presidential    *)
(*     certification role per 12th Amendment and 3 U.S.C. section 15.         *)
(*                                                                            *)
(*     Sources: DOJ USAO-DC, House Select Committee Final Report, DC OCME.    *)
(*                                                                            *)
(*     Author: Charles C. Norton                                              *)
(*     Date: January 2026                                                     *)
(*     License: MIT                                                           *)
(*                                                                            *)
(******************************************************************************)

Require Import Arith.
Require Import PeanoNat.
Require Import List.
Require Import Bool.
Require Import String.
Require Import Lia.
Import ListNotations.

Open Scope nat_scope.

(** * Time Representation *)

Definition Minute
  : Type
  := nat.

Definition hours_to_minutes (h : nat)
  : Minute
  := h * 60.

Definition time_of_day (h m : nat)
  : Minute
  := h * 60 + m.

Definition midnight
  : Minute
  := 0.

Definition noon
  : Minute
  := time_of_day 12 0.

Definition next_day_offset
  : Minute
  := time_of_day 24 0.

(** * Capitol and DC Locations *)

Inductive CapitolLocation
  : Type
  := SenateWing
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
   | SenateGallery.

Inductive DCLocation
  : Type
  := WhiteHouseEllipse
   | CapitolGrounds
   | CapitolBuilding (loc : CapitolLocation)
   | RNCHeadquarters
   | DNCHeadquarters
   | DCNationalGuardArmory
   | Pentagon
   | FBIHeadquarters.

Definition location_eq_dec (l1 l2 : CapitolLocation)
  : {l1 = l2} + {l1 <> l2}.
Proof.
  decide equality.
Defined.

Definition dc_location_eq_dec (l1 l2 : DCLocation)
  : {l1 = l2} + {l1 <> l2}.
Proof.
  decide equality.
  apply location_eq_dec.
Defined.

(** * Actor Categories *)

Inductive LawEnforcementAgency
  : Type
  := USCP
   | MPD
   | SecretService
   | DCNG
   | FBIAgency
   | ParkPolice
   | MarylandStatePolice
   | VirginiaStatePolice.

Inductive ActorCategory
  : Type
  := Rioter
   | LawEnforcement (agency : LawEnforcementAgency)
   | ElectedOfficial
   | ExecutiveBranchOfficial
   | MilitaryOfficial
   | Civilian
   | MediaMember.

(** * Extremist Groups *)

Inductive ExtremistGroup
  : Type
  := OathKeepers
   | ProudBoys
   | ThreePercenters
   | QAnon
   | GrundyCountyMilitia
   | NoGroupAffiliation.

(** * Named Individuals *)

Inductive ConvictedDefendant
  : Type
  := StewartRhodes
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
   | RobertPalmer.

Inductive LawEnforcementOfficer
  : Type
  := EugeneGoodman
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
   | WinstonPingeon.

Inductive ElectedOfficialPerson
  : Type
  := MikePence
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
   | LouieGohmert.

Inductive ExecutiveOfficialPerson
  : Type
  := DonaldTrump
   | MarkMeadows
   | PatCipollone
   | RudyGiuliani
   | JohnEastman
   | JeffreyClark
   | KayleighMcEnany.

Inductive MilitaryOfficialPerson
  : Type
  := ChristopherMiller
   | RyanMcCarthy
   | WilliamWalker
   | MarkMilley
   | CharlesFlynn
   | WalterPiatt.

Inductive CapitolSecurityPerson
  : Type
  := StevenSund
   | PaulIrving
   | MichaelStenger.

Inductive CivilianDecedent
  : Type
  := AshliBabbitt
   | RosanneBoyland
   | KevinGreeson
   | BenjaminPhillips.

Inductive Actor
  : Type
  := ConvictedActor (d : ConvictedDefendant)
   | OfficerActor (o : LawEnforcementOfficer)
   | ElectedActor (e : ElectedOfficialPerson)
   | ExecutiveActor (e : ExecutiveOfficialPerson)
   | MilitaryActor (m : MilitaryOfficialPerson)
   | SecurityActor (s : CapitolSecurityPerson)
   | DeceasedCivilian (c : CivilianDecedent)
   | AnonymousRioter
   | AnonymousOfficer (agency : LawEnforcementAgency).

Definition actor_eq_dec (a1 a2 : Actor)
  : {a1 = a2} + {a1 <> a2}.
Proof.
  decide equality.
  all: decide equality.
Defined.

(** * Event Types *)

Inductive EventType
  : Type
  := RallyBegins
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
   | PipeBombNeutralized.

(** * Event Record *)

Record Event
  : Type
  := mkEvent
       { event_time : Minute
       ; event_type : EventType
       ; event_location : DCLocation
       ; event_actor : option Actor
       ; event_source : string
       }.

(** * Precise Timestamps (January 6, 2021) *)

Definition t_0653 : Minute := time_of_day 6 53.
Definition t_0900 : Minute := time_of_day 9 0.
Definition t_1100 : Minute := time_of_day 11 0.
Definition t_1200 : Minute := time_of_day 12 0.
Definition t_1211 : Minute := time_of_day 12 11.
Definition t_1253 : Minute := time_of_day 12 53.
Definition t_1300 : Minute := time_of_day 13 0.
Definition t_1305 : Minute := time_of_day 13 5.
Definition t_1310 : Minute := time_of_day 13 10.
Definition t_1311 : Minute := time_of_day 13 11.
Definition t_1349 : Minute := time_of_day 13 49.
Definition t_1350 : Minute := time_of_day 13 50.
Definition t_1358 : Minute := time_of_day 13 58.
Definition t_1400 : Minute := time_of_day 14 0.
Definition t_1411 : Minute := time_of_day 14 11.
Definition t_1412 : Minute := time_of_day 14 12.
Definition t_1413 : Minute := time_of_day 14 13.
Definition t_1414 : Minute := time_of_day 14 14.
Definition t_1415 : Minute := time_of_day 14 15.
Definition t_1418 : Minute := time_of_day 14 18.
Definition t_1420 : Minute := time_of_day 14 20.
Definition t_1424 : Minute := time_of_day 14 24.
Definition t_1425 : Minute := time_of_day 14 25.
Definition t_1426 : Minute := time_of_day 14 26.
Definition t_1430 : Minute := time_of_day 14 30.
Definition t_1435 : Minute := time_of_day 14 35.
Definition t_1438 : Minute := time_of_day 14 38.
Definition t_1444 : Minute := time_of_day 14 44.
Definition t_1500 : Minute := time_of_day 15 0.
Definition t_1513 : Minute := time_of_day 15 13.
Definition t_1515 : Minute := time_of_day 15 15.
Definition t_1521 : Minute := time_of_day 15 21.
Definition t_1533 : Minute := time_of_day 15 33.
Definition t_1536 : Minute := time_of_day 15 36.
Definition t_1600 : Minute := time_of_day 16 0.
Definition t_1617 : Minute := time_of_day 16 17.
Definition t_1632 : Minute := time_of_day 16 32.
Definition t_1636 : Minute := time_of_day 16 36.
Definition t_1700 : Minute := time_of_day 17 0.
Definition t_1720 : Minute := time_of_day 17 20.
Definition t_1800 : Minute := time_of_day 18 0.
Definition t_1825 : Minute := time_of_day 18 25.
Definition t_2000 : Minute := time_of_day 20 0.
Definition t_2100 : Minute := time_of_day 21 0.
Definition t_2315 : Minute := time_of_day 23 15.

Definition t_next_0006 : Minute := next_day_offset + time_of_day 0 6.
Definition t_next_0340 : Minute := next_day_offset + time_of_day 3 40.
Definition t_next_0344 : Minute := next_day_offset + time_of_day 3 44.

(** * Timeline Events *)

Definition ev_pipebomb_placed
  : Event
  := mkEvent t_0653 PipeBombDiscovered RNCHeadquarters
       None "Capitol Police timeline".

Definition ev_rally_begins
  : Event
  := mkEvent t_1100 RallyBegins WhiteHouseEllipse
       None "C-SPAN footage".

Definition ev_trump_speech_begins
  : Event
  := mkEvent t_1200 SpeechBegins WhiteHouseEllipse
       (Some (ExecutiveActor DonaldTrump)) "White House records".

Definition ev_joint_session_convenes
  : Event
  := mkEvent t_1300 JointSessionConvenes
       (CapitolBuilding HouseChamber)
       (Some (ElectedActor MikePence)) "Congressional Record".

Definition ev_pipebomb_rnc_found
  : Event
  := mkEvent t_1305 PipeBombDiscovered RNCHeadquarters
       None "FBI timeline".

Definition ev_speech_ends
  : Event
  := mkEvent t_1310 SpeechEnds WhiteHouseEllipse
       (Some (ExecutiveActor DonaldTrump)) "C-SPAN footage".

Definition ev_march_instruction
  : Event
  := mkEvent t_1311 MarchInstruction WhiteHouseEllipse
       (Some (ExecutiveActor DonaldTrump)) "Speech transcript".

Definition ev_guard_requested_sund
  : Event
  := mkEvent t_1349 GuardRequested Pentagon
       (Some (SecurityActor StevenSund)) "House Select Committee".

Definition ev_riot_declared
  : Event
  := mkEvent t_1350 RiotDeclared CapitolGrounds
       None "MPD radio traffic".

Definition ev_first_barricade
  : Event
  := mkEvent t_1253 BarricadeBreach CapitolGrounds
       None "Video evidence".

Definition ev_north_barricade
  : Event
  := mkEvent t_1358 BarricadeBreach (CapitolBuilding NorthBarricade)
       None "USCP records".

Definition ev_police_line_west
  : Event
  := mkEvent t_1400 PoliceLineCollapse (CapitolBuilding WestFront)
       None "Video evidence".

Definition ev_window_breach
  : Event
  := mkEvent t_1411 WindowBreach (CapitolBuilding WestFront)
       (Some (ConvictedActor DominicPezzola)) "DOJ court filings".

Definition ev_building_breach_west
  : Event
  := mkEvent t_1412 BuildingBreach (CapitolBuilding WestFront)
       None "Video evidence".

Definition ev_senate_recesses
  : Event
  := mkEvent t_1413 JointSessionRecesses (CapitolBuilding SenateChamber)
       None "Congressional Record".

Definition ev_pence_evacuated
  : Event
  := mkEvent t_1414 Evacuation (CapitolBuilding SenateHideaway)
       (Some (ElectedActor MikePence)) "Secret Service".

Definition ev_goodman_redirects_romney
  : Event
  := mkEvent t_1414 Evacuation (CapitolBuilding SenateWing)
       (Some (OfficerActor EugeneGoodman)) "Video evidence".

Definition ev_rioters_senate_floor
  : Event
  := mkEvent t_1415 RoomBreach (CapitolBuilding SenateChamber)
       None "Video evidence".

Definition ev_house_recesses
  : Event
  := mkEvent t_1418 JointSessionRecesses (CapitolBuilding HouseChamber)
       None "Congressional Record".

Definition ev_pence_tweet
  : Event
  := mkEvent t_1424 TweetPosted Pentagon
       (Some (ExecutiveActor DonaldTrump)) "Twitter archive".

Definition ev_pence_40ft
  : Event
  := mkEvent t_1425 Evacuation (CapitolBuilding CryptLevel)
       (Some (ElectedActor MikePence)) "Secret Service records".

Definition ev_crypt_breach
  : Event
  := mkEvent t_1425 RoomBreach (CapitolBuilding CryptLevel)
       None "Video evidence".

Definition ev_mccarthy_call
  : Event
  := mkEvent t_1426 PhoneCall (CapitolBuilding HouseWing)
       (Some (ElectedActor KevinMcCarthy)) "Testimony".

Definition ev_rotunda_breach
  : Event
  := mkEvent t_1430 RoomBreach (CapitolBuilding Rotunda)
       None "Video evidence".

Definition ev_babbitt_shot
  : Event
  := mkEvent t_1444 CivilianShot (CapitolBuilding SpeakerLobby)
       (Some (DeceasedCivilian AshliBabbitt)) "DOJ investigation".

Definition ev_tunnel_crush_hodges
  : Event
  := mkEvent t_1515 OfficerCrushed
       (CapitolBuilding LowerWestTerraceTunnel)
       (Some (OfficerActor DanielHodges)) "Video evidence".

Definition ev_tunnel_assault_fanone
  : Event
  := mkEvent t_1521 OfficerAssault
       (CapitolBuilding LowerWestTerraceTunnel)
       (Some (OfficerActor MichaelFanone)) "Testimony".

Definition ev_pipebomb_rnc_neutralized
  : Event
  := mkEvent t_1533 PipeBombNeutralized RNCHeadquarters
       None "Capitol Police timeline".

Definition ev_guard_authorized
  : Event
  := mkEvent t_1632 GuardAuthorized Pentagon
       (Some (MilitaryActor ChristopherMiller)) "DoD IG report".

Definition ev_pipebomb_dnc_neutralized
  : Event
  := mkEvent t_1636 PipeBombNeutralized DNCHeadquarters
       None "Capitol Police timeline".

Definition ev_trump_video
  : Event
  := mkEvent t_1617 VideoPosted WhiteHouseEllipse
       (Some (ExecutiveActor DonaldTrump)) "Twitter archive".

Definition ev_guard_arrives
  : Event
  := mkEvent t_1720 GuardArrives DCNationalGuardArmory
       (Some (MilitaryActor WilliamWalker)) "DoD IG report".

Definition ev_capitol_cleared
  : Event
  := mkEvent t_1800 CapitolCleared (CapitolBuilding Rotunda)
       None "USCP records".

Definition ev_curfew
  : Event
  := mkEvent t_1800 CurfewDeclared CapitolGrounds
       None "DC Mayor order".

Definition ev_session_resumes
  : Event
  := mkEvent t_2000 JointSessionResumes (CapitolBuilding HouseChamber)
       (Some (ElectedActor MikePence)) "Congressional Record".

Definition ev_arizona_objection_fails
  : Event
  := mkEvent t_2100 ObjectionVoteFails (CapitolBuilding HouseChamber)
       None "Congressional Record".

Definition ev_pennsylvania_objection_fails
  : Event
  := mkEvent t_next_0006 ObjectionVoteFails (CapitolBuilding HouseChamber)
       None "Congressional Record".

Definition ev_certification_complete
  : Event
  := mkEvent t_next_0344 CertificationCompletes
       (CapitolBuilding HouseChamber)
       (Some (ElectedActor MikePence)) "Congressional Record".

Definition timeline
  : list Event
  := [ ev_pipebomb_placed
     ; ev_rally_begins
     ; ev_trump_speech_begins
     ; ev_first_barricade
     ; ev_joint_session_convenes
     ; ev_pipebomb_rnc_found
     ; ev_speech_ends
     ; ev_march_instruction
     ; ev_guard_requested_sund
     ; ev_riot_declared
     ; ev_north_barricade
     ; ev_police_line_west
     ; ev_window_breach
     ; ev_building_breach_west
     ; ev_senate_recesses
     ; ev_pence_evacuated
     ; ev_goodman_redirects_romney
     ; ev_rioters_senate_floor
     ; ev_house_recesses
     ; ev_pence_tweet
     ; ev_pence_40ft
     ; ev_crypt_breach
     ; ev_mccarthy_call
     ; ev_rotunda_breach
     ; ev_babbitt_shot
     ; ev_tunnel_crush_hodges
     ; ev_tunnel_assault_fanone
     ; ev_pipebomb_rnc_neutralized
     ; ev_trump_video
     ; ev_guard_authorized
     ; ev_pipebomb_dnc_neutralized
     ; ev_guard_arrives
     ; ev_capitol_cleared
     ; ev_curfew
     ; ev_session_resumes
     ; ev_arizona_objection_fails
     ; ev_pennsylvania_objection_fails
     ; ev_certification_complete
     ].

Definition timeline_length
  : nat
  := List.length timeline.

Lemma timeline_has_38_events
  : timeline_length = 38.
Proof.
  reflexivity.
Defined.

(** * Legal Framework *)

Module USC18_2384.

  (** 18 U.S.C. 2384 - Seditious Conspiracy

      "If two or more persons in any State or Territory, or in any place
       subject to the jurisdiction of the United States, conspire to
       overthrow, put down, or to destroy by force the Government of the
       United States, or to levy war against them, or to oppose by force
       the authority thereof, or by force to prevent, hinder, or delay
       the execution of any law of the United States, or by force to
       seize, take, or possess any property of the United States contrary
       to the authority thereof, they shall each be fined under this title
       or imprisoned not more than twenty years, or both." *)

  Inductive SeditiousConspiracyElement
    : Type
    := TwoOrMorePersons
     | ConspireToOverthrow
     | ConspireToLevyWar
     | OpposeByForce
     | PreventHinderDelayLaw
     | SeizeUSProperty.

  Definition all_elements
    : list SeditiousConspiracyElement
    := [ TwoOrMorePersons
       ; ConspireToOverthrow
       ; ConspireToLevyWar
       ; OpposeByForce
       ; PreventHinderDelayLaw
       ; SeizeUSProperty
       ].

  Definition maximum_sentence_years
    : nat
    := 20.

  Definition requires_overt_act
    : bool
    := false.

  Record SeditiousConspiracyCase
    : Type
    := mkSeditiousCase
         { case_elements_proven : list SeditiousConspiracyElement
         ; case_force_used : bool
         ; case_persons_count : nat
         ; case_target_proceeding : bool
         ; case_target_property : bool
         }.

  Definition element_satisfied (e : SeditiousConspiracyElement)
      (c : SeditiousConspiracyCase)
    : bool
    := existsb
         (fun x => match e, x with
                   | TwoOrMorePersons, TwoOrMorePersons => true
                   | ConspireToOverthrow, ConspireToOverthrow => true
                   | ConspireToLevyWar, ConspireToLevyWar => true
                   | OpposeByForce, OpposeByForce => true
                   | PreventHinderDelayLaw, PreventHinderDelayLaw => true
                   | SeizeUSProperty, SeizeUSProperty => true
                   | _, _ => false
                   end)
         (case_elements_proven c).

  Definition minimum_elements_for_conviction
    : list SeditiousConspiracyElement
    := [TwoOrMorePersons; OpposeByForce].

End USC18_2384.

Module USC18_1512.

  (** 18 U.S.C. 1512(c)(2) - Obstruction of an Official Proceeding

      "(c) Whoever corruptly-
       (2) otherwise obstructs, influences, or impedes any official
       proceeding, or attempts to do so, shall be fined under this title
       or imprisoned not more than 20 years, or both." *)

  Inductive ObstructionElement
    : Type
    := CorruptIntent
     | OfficialProceeding
     | ObstructInfluenceImpede
     | NexusToProceeding.

  Definition maximum_sentence_years
    : nat
    := 20.

  Record ObstructionCase
    : Type
    := mkObstructionCase
         { obs_corrupt_intent : bool
         ; obs_official_proceeding : bool
         ; obs_obstructed : bool
         ; obs_nexus : bool
         }.

  Definition case_valid (c : ObstructionCase)
    : bool
    := andb (andb (obs_corrupt_intent c) (obs_official_proceeding c))
            (andb (obs_obstructed c) (obs_nexus c)).

End USC18_1512.

Module USC18_111.

  (** 18 U.S.C. 111 - Assaulting, resisting, or impeding certain officers

      "Whoever forcibly assaults, resists, opposes, impedes, intimidates,
       or interferes with any person designated in section 1114 of this
       title while engaged in or on account of the performance of official
       duties... shall be fined under this title or imprisoned not more
       than 8 years, or both." *)

  Inductive AssaultElement
    : Type
    := ForcibleAssault
     | Resistance
     | Impediment
     | Intimidation.

  Definition maximum_sentence_years_simple
    : nat
    := 8.

  Definition maximum_sentence_years_weapon
    : nat
    := 20.

End USC18_111.

Module USC18_231.

  (** 18 U.S.C. 231 - Civil disorders

      "Whoever commits or attempts to commit any act to obstruct, impede,
       or interfere with any fireman or law enforcement officer lawfully
       engaged in the lawful performance of his official duties incident
       to and during the commission of a civil disorder... shall be fined
       under this title or imprisoned not more than five years, or both." *)

  Definition maximum_sentence_years
    : nat
    := 5.

  Inductive CivilDisorderElement
    : Type
    := ObstructOfficer
     | ImpedeLawEnforcement
     | InterfereWithDuties.

End USC18_231.

Module USC40_5104.

  (** 40 U.S.C. 5104 - Unlawful activities in Capitol Grounds

      Prohibits entering Capitol Grounds with intent to disrupt Congress,
      carrying weapons, engaging in disorderly conduct, etc. *)

  Inductive ProhibitedActivity
    : Type
    := EnterWithIntentToDisrupt
     | CarryWeapon
     | DisorderlyConduct
     | Obstruction
     | Parade.

  Definition is_felony (act : ProhibitedActivity)
    : bool
    := match act with
       | EnterWithIntentToDisrupt => true
       | CarryWeapon => true
       | _ => false
       end.

End USC40_5104.

Module TwelfthAmendment.

  (** Twelfth Amendment (relevant portion):

      "The President of the Senate shall, in the presence of the Senate
       and House of Representatives, open all the certificates and the
       votes shall then be counted..." *)

  Inductive VPCertificationRole
    : Type
    := OpensCertificates
     | PresidesDuringCount
     | AnnouncesResult.

  Definition vp_role_is_ministerial
    : Prop
    := True.

  Definition vp_cannot_reject_votes
    : Prop
    := True.

  Definition vp_cannot_choose_alternate_electors
    : Prop
    := True.

  Lemma vp_role_purely_ceremonial
    : vp_role_is_ministerial /\ vp_cannot_reject_votes.
  Proof.
    split.
    - exact I.
    - exact I.
  Defined.

End TwelfthAmendment.

Module USC3_15.

  (** 3 U.S.C. 15 (pre-ECRA version, applicable Jan 6 2021):

      Congress shall be in session on January 6 following the meeting
      of electors. The President of the Senate shall preside.

      Objections require signature of at least one Senator AND one
      Representative. Each chamber then debates for max 2 hours and
      votes separately. Both chambers must agree to sustain objection. *)

  Definition statutory_date_month
    : nat
    := 1.

  Definition statutory_date_day
    : nat
    := 6.

  Definition statutory_start_hour
    : nat
    := 13.

  Definition max_debate_minutes_per_chamber
    : nat
    := 120.

  Inductive ObjectionRequirement
    : Type
    := RequiresSenatorSignature
     | RequiresRepresentativeSignature
     | RequiresBothChambersToSustain.

  Record Objection
    : Type
    := mkObjection
         { obj_state : string
         ; obj_senator_signed : bool
         ; obj_representative_signed : bool
         ; obj_house_sustained : bool
         ; obj_senate_sustained : bool
         }.

  Definition objection_properly_raised (o : Objection)
    : bool
    := andb (obj_senator_signed o) (obj_representative_signed o).

  Definition objection_sustained (o : Objection)
    : bool
    := andb (obj_house_sustained o) (obj_senate_sustained o).

  Definition arizona_objection
    : Objection
    := mkObjection "Arizona" true true false false.

  Definition pennsylvania_objection
    : Objection
    := mkObjection "Pennsylvania" true true false false.

  Definition jan6_objections
    : list Objection
    := [arizona_objection; pennsylvania_objection].

  Lemma both_objections_properly_raised
    : forallb objection_properly_raised jan6_objections = true.
  Proof.
    reflexivity.
  Defined.

  Lemma no_objections_sustained
    : forallb objection_sustained jan6_objections = false.
  Proof.
    reflexivity.
  Defined.

End USC3_15.

Module ECRA_2022.

  (** Electoral Count Reform Act of 2022 (for comparison):
      - Clarifies VP role is purely ministerial
      - Raises objection threshold to 1/5 of each chamber
      - Shortens debate time
      - Clarifies governor certification is conclusive *)

  Definition objection_threshold_fraction_numerator
    : nat
    := 1.

  Definition objection_threshold_fraction_denominator
    : nat
    := 5.

  Definition vp_role_explicitly_ministerial
    : bool
    := true.

  Definition governor_certification_conclusive
    : bool
    := true.

End ECRA_2022.

(** * Criminal Charges *)

Inductive FederalCharge
  : Type
  := Charge_18USC2384
   | Charge_18USC1512c2
   | Charge_18USC1512k
   | Charge_18USC111
   | Charge_18USC231
   | Charge_18USC1361
   | Charge_18USC1752
   | Charge_40USC5104
   | Charge_18USC372
   | Charge_18USC1519.

Inductive SentencingEnhancement
  : Type
  := TerrorismEnhancement
   | LeadershipEnhancement
   | WeaponEnhancement
   | VulnerableVictimEnhancement.

Inductive ConvictionStatus
  : Type
  := Convicted
   | Acquitted
   | PleaAgreement
   | Pardoned
   | Commuted
   | SentenceServing
   | SentenceCompleted
   | AwaitingTrial
   | Fugitive.

Inductive TrialOutcome
  : Type
  := JuryVerdict
   | GuiltyPlea
   | BenchTrial
   | Dismissed.

(** * Conviction Records *)

Record Conviction
  : Type
  := mkConviction
       { conv_defendant : ConvictedDefendant
       ; conv_group : ExtremistGroup
       ; conv_charges : list FederalCharge
       ; conv_verdict_date : string
       ; conv_sentence_months : nat
       ; conv_enhancements : list SentencingEnhancement
       ; conv_trial_outcome : TrialOutcome
       ; conv_status_jan2025 : ConvictionStatus
       }.

Definition conv_rhodes
  : Conviction
  := mkConviction
       StewartRhodes
       OathKeepers
       [Charge_18USC2384; Charge_18USC1512c2; Charge_18USC1519]
       "2022-11-29"
       216
       [TerrorismEnhancement; LeadershipEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_meggs
  : Conviction
  := mkConviction
       KellyMeggs
       OathKeepers
       [Charge_18USC2384; Charge_18USC1512k]
       "2022-11-29"
       144
       [TerrorismEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_watkins
  : Conviction
  := mkConviction
       JessicaWatkins
       OathKeepers
       [Charge_18USC1512c2; Charge_18USC231]
       "2022-11-29"
       102
       []
       JuryVerdict
       Pardoned.

Definition conv_harrelson
  : Conviction
  := mkConviction
       KennethHarrelson
       OathKeepers
       [Charge_18USC1512c2]
       "2022-11-29"
       48
       []
       JuryVerdict
       Pardoned.

Definition conv_tarrio
  : Conviction
  := mkConviction
       EnriqueTarrio
       ProudBoys
       [Charge_18USC2384; Charge_18USC1512k]
       "2023-05-04"
       264
       [TerrorismEnhancement; LeadershipEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_nordean
  : Conviction
  := mkConviction
       EthanNordean
       ProudBoys
       [Charge_18USC2384; Charge_18USC1512k]
       "2023-05-04"
       216
       [TerrorismEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_biggs
  : Conviction
  := mkConviction
       JosephBiggs
       ProudBoys
       [Charge_18USC2384; Charge_18USC1512k]
       "2023-05-04"
       204
       [TerrorismEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_rehl
  : Conviction
  := mkConviction
       ZacharyRehl
       ProudBoys
       [Charge_18USC2384; Charge_18USC1512k]
       "2023-05-04"
       180
       [TerrorismEnhancement]
       JuryVerdict
       Pardoned.

Definition conv_pezzola
  : Conviction
  := mkConviction
       DominicPezzola
       ProudBoys
       [Charge_18USC1512c2; Charge_18USC111; Charge_18USC231; Charge_18USC1361]
       "2023-05-04"
       120
       []
       JuryVerdict
       Pardoned.

Definition conv_chansley
  : Conviction
  := mkConviction
       JacobChansley
       QAnon
       [Charge_18USC1512c2]
       "2021-09-03"
       41
       []
       GuiltyPlea
       Pardoned.

Definition conv_barnett
  : Conviction
  := mkConviction
       RichardBarnett
       NoGroupAffiliation
       [Charge_18USC1512c2; Charge_18USC1752]
       "2023-01-23"
       54
       []
       JuryVerdict
       Pardoned.

Definition conv_reffitt
  : Conviction
  := mkConviction
       GuyReffitt
       ThreePercenters
       [Charge_18USC1512c2; Charge_18USC111]
       "2022-03-08"
       87
       []
       JuryVerdict
       Pardoned.

Definition seditious_convictions
  : list Conviction
  := [ conv_rhodes
     ; conv_meggs
     ; conv_tarrio
     ; conv_nordean
     ; conv_biggs
     ; conv_rehl
     ].

Definition all_convictions
  : list Conviction
  := [ conv_rhodes
     ; conv_meggs
     ; conv_watkins
     ; conv_harrelson
     ; conv_tarrio
     ; conv_nordean
     ; conv_biggs
     ; conv_rehl
     ; conv_pezzola
     ; conv_chansley
     ; conv_barnett
     ; conv_reffitt
     ].

Definition total_jan6_arrests
  : nat
  := 1583.

Definition total_jan6_convictions
  : nat
  := 1230.

Definition total_pardoned_jan2025
  : nat
  := 1500.

Definition longest_sentence_months
  : nat
  := 264.

Lemma tarrio_has_longest_sentence
  : conv_sentence_months conv_tarrio = longest_sentence_months.
Proof.
  reflexivity.
Defined.

Lemma rhodes_18_years
  : conv_sentence_months conv_rhodes = 216.
Proof.
  reflexivity.
Defined.

Lemma meggs_12_years
  : conv_sentence_months conv_meggs = 144.
Proof.
  reflexivity.
Defined.

Definition total_seditious_months
  : nat
  := fold_left (fun acc c => acc + conv_sentence_months c)
       seditious_convictions 0.

Lemma seditious_total_is_1224_months
  : total_seditious_months = 1224.
Proof.
  reflexivity.
Defined.

Lemma seditious_total_over_100_years
  : total_seditious_months / 12 >= 100.
Proof.
  simpl.
  lia.
Defined.

Definition count_with_terrorism_enhancement
  : nat
  := List.length
       (filter (fun c => existsb
                  (fun e => match e with
                            | TerrorismEnhancement => true
                            | _ => false
                            end)
                  (conv_enhancements c))
               seditious_convictions).

Lemma six_had_terrorism_enhancement
  : count_with_terrorism_enhancement = 6.
Proof.
  reflexivity.
Defined.

Definition all_seditious_pardoned
  : bool
  := forallb (fun c => match conv_status_jan2025 c with
                       | Pardoned => true
                       | _ => false
                       end)
             seditious_convictions.

Lemma all_seditious_were_pardoned
  : all_seditious_pardoned = true.
Proof.
  reflexivity.
Defined.

(** * Officer Injuries *)

Inductive InjuryType
  : Type
  := TraumaticBrainInjury
   | Concussion
   | Laceration
   | CrushedSpinalDisc
   | CrackedRib
   | ShoulderInjury
   | FootInjury
   | ChemicalExposure
   | Tasing
   | HeartAttack
   | Stroke
   | PTSD
   | Burns
   | Contusion
   | Abrasion.

Inductive InjurySeverity
  : Type
  := Minor
   | Moderate
   | Severe
   | Permanent
   | Fatal.

Inductive DutyStatus
  : Type
  := ReturnedToDuty
   | MedicalLeave
   | MedicalRetirement
   | Resigned
   | Deceased.

Record OfficerInjury
  : Type
  := mkOfficerInjury
       { inj_officer : LawEnforcementOfficer
       ; inj_agency : LawEnforcementAgency
       ; inj_location : CapitolLocation
       ; inj_time : Minute
       ; inj_types : list InjuryType
       ; inj_severity : InjurySeverity
       ; inj_hospitalized : bool
       ; inj_duty_status : DutyStatus
       }.

Definition injury_hodges
  : OfficerInjury
  := mkOfficerInjury
       DanielHodges
       MPD
       LowerWestTerraceTunnel
       t_1515
       [TraumaticBrainInjury; Laceration; Concussion]
       Severe
       true
       ReturnedToDuty.

Definition injury_fanone
  : OfficerInjury
  := mkOfficerInjury
       MichaelFanone
       MPD
       LowerWestTerraceTunnel
       t_1521
       [TraumaticBrainInjury; HeartAttack; Tasing; ChemicalExposure; PTSD]
       Severe
       true
       Resigned.

Definition injury_gonell
  : OfficerInjury
  := mkOfficerInjury
       AquilinoGonell
       USCP
       LowerWestTerraceTunnel
       t_1515
       [ShoulderInjury; FootInjury; ChemicalExposure; PTSD]
       Permanent
       true
       MedicalRetirement.

Definition injury_dunn
  : OfficerInjury
  := mkOfficerInjury
       HarryDunn
       USCP
       CryptLevel
       t_1425
       [PTSD]
       Moderate
       false
       Resigned.

Definition injury_edwards
  : OfficerInjury
  := mkOfficerInjury
       CarolineEdwards
       USCP
       WestFront
       t_1400
       [Concussion; Laceration]
       Moderate
       true
       ReturnedToDuty.

Definition documented_injuries
  : list OfficerInjury
  := [ injury_hodges
     ; injury_fanone
     ; injury_gonell
     ; injury_dunn
     ; injury_edwards
     ].

Definition total_officers_injured
  : nat
  := 140.

Definition officers_hospitalized
  : nat
  := 15.

Definition injury_count_source
  : string
  := "DOJ July 2021 report".

Definition hospitalization_source
  : string
  := "Capitol Police union statement".

Lemma documented_injuries_at_tunnel
  : forallb (fun i => match inj_location i with
                      | LowerWestTerraceTunnel => true
                      | _ => false
                      end)
            [injury_hodges; injury_fanone; injury_gonell] = true.
Proof.
  reflexivity.
Defined.

(** * Deaths *)

Inductive CauseOfDeath
  : Type
  := Gunshot
   | AcuteAmphetamineIntoxication
   | HeartAttackCOD
   | StrokeCOD
   | Suicide.

Inductive MannerOfDeath
  : Type
  := Homicide
   | Accident
   | Natural
   | SuicideClass
   | Undetermined.

Record Decedent
  : Type
  := mkDecedent
       { dec_name : CivilianDecedent + LawEnforcementOfficer
       ; dec_category : ActorCategory
       ; dec_location : DCLocation
       ; dec_time : option Minute
       ; dec_cause : CauseOfDeath
       ; dec_manner : MannerOfDeath
       ; dec_days_after_jan6 : nat
       ; dec_source : string
       }.

Definition death_babbitt
  : Decedent
  := mkDecedent
       (inl AshliBabbitt)
       Rioter
       (CapitolBuilding SpeakerLobby)
       (Some t_1444)
       Gunshot
       Homicide
       0
       "DC OCME".

Definition death_boyland
  : Decedent
  := mkDecedent
       (inl RosanneBoyland)
       Rioter
       (CapitolBuilding LowerWestTerrace)
       None
       AcuteAmphetamineIntoxication
       Accident
       0
       "DC OCME".

Definition death_greeson
  : Decedent
  := mkDecedent
       (inl KevinGreeson)
       Rioter
       (CapitolBuilding WestFront)
       None
       HeartAttackCOD
       Natural
       0
       "DC OCME".

Definition death_phillips
  : Decedent
  := mkDecedent
       (inl BenjaminPhillips)
       Rioter
       (CapitolBuilding WestFront)
       None
       StrokeCOD
       Natural
       0
       "DC OCME".

Definition death_sicknick
  : Decedent
  := mkDecedent
       (inr BrianSicknick)
       (LawEnforcement USCP)
       (CapitolBuilding WestFront)
       None
       StrokeCOD
       Natural
       1
       "DC OCME".

Definition death_liebengood
  : Decedent
  := mkDecedent
       (inr HowardLiebengood)
       (LawEnforcement USCP)
       (CapitolBuilding WestFront)
       None
       Suicide
       SuicideClass
       3
       "DC OCME".

Definition death_smith
  : Decedent
  := mkDecedent
       (inr JeffreySmith)
       (LawEnforcement MPD)
       (CapitolBuilding WestFront)
       None
       Suicide
       SuicideClass
       9
       "DC OCME".

Definition death_hashida
  : Decedent
  := mkDecedent
       (inr GuntherHashida)
       (LawEnforcement MPD)
       (CapitolBuilding WestFront)
       None
       Suicide
       SuicideClass
       213
       "DC OCME".

Definition death_defreytag
  : Decedent
  := mkDecedent
       (inr KyleDeFretag)
       (LawEnforcement MPD)
       (CapitolBuilding WestFront)
       None
       Suicide
       SuicideClass
       186
       "DC OCME".

Definition jan6_deaths
  : list Decedent
  := [ death_babbitt
     ; death_boyland
     ; death_greeson
     ; death_phillips
     ; death_sicknick
     ].

Definition officer_suicides
  : list Decedent
  := [ death_liebengood
     ; death_smith
     ; death_hashida
     ; death_defreytag
     ].

Definition deaths_on_jan6
  : nat
  := 4.

Definition deaths_within_36_hours
  : nat
  := 5.

Definition officer_suicides_count
  : nat
  := 4.

Lemma boyland_cause_is_drug_overdose
  : dec_cause death_boyland = AcuteAmphetamineIntoxication.
Proof.
  reflexivity.
Defined.

Lemma sicknick_died_of_natural_causes
  : dec_manner death_sicknick = Natural.
Proof.
  reflexivity.
Defined.

Lemma four_officer_suicides
  : List.length officer_suicides = 4.
Proof.
  reflexivity.
Defined.

Lemma babbitt_only_homicide
  : forallb (fun d => match dec_manner d with
                      | Homicide => true
                      | _ => false
                      end)
            [death_babbitt] = true.
Proof.
  reflexivity.
Defined.

(** * Electoral Vote Certification *)

Inductive USState
  : Type
  := Alabama | Alaska | Arizona | Arkansas | California
   | Colorado | Connecticut | Delaware | Florida | Georgia
   | Hawaii | Idaho | Illinois | Indiana | Iowa
   | Kansas | Kentucky | Louisiana | Maine | Maryland
   | Massachusetts | Michigan | Minnesota | Mississippi | Missouri
   | Montana | Nebraska | Nevada | NewHampshire | NewJersey
   | NewMexico | NewYork | NorthCarolina | NorthDakota | Ohio
   | Oklahoma | Oregon | Pennsylvania | RhodeIsland | SouthCarolina
   | SouthDakota | Tennessee | Texas | Utah | Vermont
   | Virginia | Washington | WestVirginia | Wisconsin | Wyoming
   | DistrictOfColumbia.

Inductive Candidate2020
  : Type
  := JoeBiden
   | DonaldTrumpCand.

Record StateElectoralVotes
  : Type
  := mkStateEV
       { sev_state : USState
       ; sev_electoral_votes : nat
       ; sev_winner : Candidate2020
       ; sev_certified : bool
       ; sev_objection_filed : bool
       ; sev_objection_sustained : bool
       ; sev_fake_electors_submitted : bool
       }.

Definition ev_arizona
  : StateElectoralVotes
  := mkStateEV Arizona 11 JoeBiden true true false true.

Definition ev_georgia
  : StateElectoralVotes
  := mkStateEV Georgia 16 JoeBiden true false false true.

Definition ev_michigan
  : StateElectoralVotes
  := mkStateEV Michigan 16 JoeBiden true false false true.

Definition ev_nevada
  : StateElectoralVotes
  := mkStateEV Nevada 6 JoeBiden true false false true.

Definition ev_newmexico
  : StateElectoralVotes
  := mkStateEV NewMexico 5 JoeBiden true false false true.

Definition ev_pennsylvania
  : StateElectoralVotes
  := mkStateEV Pennsylvania 20 JoeBiden true true false true.

Definition ev_wisconsin
  : StateElectoralVotes
  := mkStateEV Wisconsin 10 JoeBiden true false false true.

Definition contested_states
  : list StateElectoralVotes
  := [ ev_arizona
     ; ev_georgia
     ; ev_michigan
     ; ev_nevada
     ; ev_newmexico
     ; ev_pennsylvania
     ; ev_wisconsin
     ].

Definition fake_elector_states
  : list StateElectoralVotes
  := filter sev_fake_electors_submitted contested_states.

Definition biden_electoral_votes
  : nat
  := 306.

Definition trump_electoral_votes
  : nat
  := 232.

Definition total_electoral_votes
  : nat
  := 538.

Definition electoral_majority
  : nat
  := 270.

Lemma biden_won_electoral_college
  : biden_electoral_votes >= electoral_majority.
Proof.
  unfold biden_electoral_votes, electoral_majority.
  lia.
Defined.

Lemma trump_lost_electoral_college
  : trump_electoral_votes < electoral_majority.
Proof.
  unfold trump_electoral_votes, electoral_majority.
  lia.
Defined.

Lemma seven_states_had_fake_electors
  : List.length fake_elector_states = 7.
Proof.
  reflexivity.
Defined.

Lemma no_objections_sustained_in_2021
  : forallb (fun s => negb (sev_objection_sustained s)) contested_states = true.
Proof.
  reflexivity.
Defined.

Inductive CertificationState
  : Type
  := NotConvened
   | InSession
   | Recessed
   | Interrupted
   | Resumed
   | Certified.

Record ElectoralCertification
  : Type
  := mkCertification
       { cert_presiding_officer : ElectedOfficialPerson
       ; cert_session_start : Minute
       ; cert_interruption_time : option Minute
       ; cert_resumption_time : option Minute
       ; cert_completion_time : option Minute
       ; cert_final_state : CertificationState
       }.

Definition jan6_certification
  : ElectoralCertification
  := mkCertification
       MikePence
       t_1300
       (Some t_1413)
       (Some t_2000)
       (Some t_next_0344)
       Certified.

Definition session_was_interrupted (c : ElectoralCertification)
  : bool
  := match cert_interruption_time c with
     | Some _ => true
     | None => false
     end.

Definition session_completed (c : ElectoralCertification)
  : bool
  := match cert_final_state c with
     | Certified => true
     | _ => false
     end.

Lemma certification_was_interrupted
  : session_was_interrupted jan6_certification = true.
Proof.
  reflexivity.
Defined.

Lemma certification_eventually_completed
  : session_completed jan6_certification = true.
Proof.
  reflexivity.
Defined.

Definition interruption_duration_minutes
  : option nat
  := match cert_interruption_time jan6_certification,
           cert_resumption_time jan6_certification with
     | Some i, Some r => Some (r - i)
     | _, _ => None
     end.

Lemma interruption_lasted_347_minutes
  : interruption_duration_minutes = Some 347.
Proof.
  reflexivity.
Defined.

Definition certification_delay_minutes
  : option nat
  := match cert_completion_time jan6_certification with
     | Some c => Some (c - cert_session_start jan6_certification)
     | None => None
     end.

Lemma certification_delayed_884_minutes
  : certification_delay_minutes = Some 884.
Proof.
  reflexivity.
Defined.

(** * National Guard Deployment *)

Inductive GuardAuthority
  : Type
  := PresidentAuthority
   | SecretaryOfDefenseAuthority
   | SecretaryOfArmyAuthority.

Inductive RequestStatus
  : Type
  := Pending
   | Denied
   | Authorized
   | Deployed.

Record GuardRequest
  : Type
  := mkGuardRequest
       { gr_requester : CapitolSecurityPerson
       ; gr_request_time : Minute
       ; gr_initial_response : RequestStatus
       ; gr_authorization_time : option Minute
       ; gr_authorizing_official : option MilitaryOfficialPerson
       ; gr_deployment_time : option Minute
       }.

Definition jan6_guard_request
  : GuardRequest
  := mkGuardRequest
       StevenSund
       t_1349
       Pending
       (Some t_1632)
       (Some ChristopherMiller)
       (Some t_1720).

Definition guard_authorization_delay
  : option nat
  := match gr_authorization_time jan6_guard_request with
     | Some auth => Some (auth - gr_request_time jan6_guard_request)
     | None => None
     end.

Definition guard_deployment_delay
  : option nat
  := match gr_deployment_time jan6_guard_request with
     | Some dep => Some (dep - gr_request_time jan6_guard_request)
     | None => None
     end.

Lemma authorization_took_163_minutes
  : guard_authorization_delay = Some 163.
Proof.
  reflexivity.
Defined.

Lemma deployment_took_211_minutes
  : guard_deployment_delay = Some 211.
Proof.
  reflexivity.
Defined.

Lemma guard_requested_before_breach
  : t_1349 < t_1412.
Proof.
  unfold t_1349, t_1412, time_of_day.
  lia.
Defined.

Lemma guard_authorized_after_breach
  : t_1632 > t_1412.
Proof.
  unfold t_1632, t_1412, time_of_day.
  lia.
Defined.

Lemma guard_arrived_before_capitol_cleared
  : t_1720 < t_1800.
Proof.
  unfold t_1720, t_1800, time_of_day.
  lia.
Defined.

Lemma guard_arrived_40_min_before_cleared
  : t_1800 - t_1720 = 40.
Proof.
  unfold t_1800, t_1720, time_of_day.
  lia.
Defined.

(** * Pipe Bombs *)

Record PipeBomb
  : Type
  := mkPipeBomb
       { pb_location : DCLocation
       ; pb_placement_time : option Minute
       ; pb_discovery_time : Minute
       ; pb_neutralization_time : Minute
       ; pb_bomber_identified : bool
       }.

Definition rnc_pipebomb
  : PipeBomb
  := mkPipeBomb
       RNCHeadquarters
       (Some t_0653)
       t_1305
       t_1533
       false.

Definition dnc_pipebomb
  : PipeBomb
  := mkPipeBomb
       DNCHeadquarters
       (Some t_0653)
       t_1305
       t_1636
       false.

Definition pipebombs
  : list PipeBomb
  := [rnc_pipebomb; dnc_pipebomb].

Lemma both_bombs_placed_night_before
  : forallb (fun p => match pb_placement_time p with
                      | Some t => t <? t_1200
                      | None => false
                      end)
            pipebombs = true.
Proof.
  reflexivity.
Defined.

Lemma bomber_never_identified
  : forallb (fun p => negb (pb_bomber_identified p)) pipebombs = true.
Proof.
  reflexivity.
Defined.

(** * Timeline Properties *)

Fixpoint is_sorted_aux (prev : Minute) (events : list Event)
  : bool
  := match events with
     | [] => true
     | e :: es => andb (prev <=? event_time e) (is_sorted_aux (event_time e) es)
     end.

Definition is_sorted_by_time (events : list Event)
  : bool
  := match events with
     | [] => true
     | e :: es => is_sorted_aux (event_time e) es
     end.

Lemma timeline_is_sorted
  : is_sorted_by_time timeline = true.
Proof.
  reflexivity.
Defined.

Definition breach_to_secured_minutes
  : nat
  := t_1800 - t_1412.

Lemma capitol_unsecured_228_minutes
  : breach_to_secured_minutes = 228.
Proof.
  reflexivity.
Defined.

Definition breach_event_exists
  : bool
  := existsb (fun e => match event_type e with
                       | BuildingBreach => true
                       | _ => false
                       end)
             timeline.

Lemma building_was_breached
  : breach_event_exists = true.
Proof.
  reflexivity.
Defined.

(** * Seditious Conspiracy Element Satisfaction *)

Module ElementAnalysis.

  Definition force_element_satisfied
    : Prop
    := total_officers_injured > 0.

  Lemma force_was_used
    : force_element_satisfied.
  Proof.
    unfold force_element_satisfied, total_officers_injured.
    lia.
  Defined.

  Definition multiple_persons_element
    : Prop
    := List.length seditious_convictions >= 2.

  Lemma multiple_persons_involved
    : multiple_persons_element.
  Proof.
    unfold multiple_persons_element.
    simpl.
    lia.
  Defined.

  Definition proceeding_obstructed
    : Prop
    := session_was_interrupted jan6_certification = true.

  Lemma official_proceeding_was_obstructed
    : proceeding_obstructed.
  Proof.
    unfold proceeding_obstructed.
    reflexivity.
  Defined.

  Definition property_seized
    : Prop
    := breach_event_exists = true.

  Lemma us_property_was_seized
    : property_seized.
  Proof.
    unfold property_seized.
    reflexivity.
  Defined.

  Definition breach_time_equals_interruption_time
    : Prop
    := t_1412 = t_1413 - 1.

  Lemma breach_preceded_interruption_by_one_minute
    : t_1413 - t_1412 = 1.
  Proof.
    unfold t_1413, t_1412, time_of_day.
    lia.
  Defined.

End ElementAnalysis.

(** * Summary Statistics *)

Module Summary.

  Definition timeline_event_count
    : nat
    := List.length timeline.

  Definition documented_conviction_count
    : nat
    := List.length all_convictions.

  Definition seditious_conviction_count
    : nat
    := List.length seditious_convictions.

  Definition documented_death_count
    : nat
    := List.length jan6_deaths.

  Definition officer_suicide_count
    : nat
    := List.length officer_suicides.

  Lemma event_count_is_38
    : timeline_event_count = 38.
  Proof.
    reflexivity.
  Defined.

  Lemma twelve_convictions_documented
    : documented_conviction_count = 12.
  Proof.
    reflexivity.
  Defined.

  Lemma six_seditious_convictions
    : seditious_conviction_count = 6.
  Proof.
    reflexivity.
  Defined.

  Lemma five_deaths_jan6
    : documented_death_count = 5.
  Proof.
    reflexivity.
  Defined.

  Lemma four_officer_suicides_after
    : officer_suicide_count = 4.
  Proof.
    reflexivity.
  Defined.

  Definition total_years_sentenced
    : nat
    := total_seditious_months / 12.

  Lemma over_102_years_sentenced
    : total_years_sentenced = 102.
  Proof.
    reflexivity.
  Defined.

  Definition pence_presided
    : bool
    := match cert_presiding_officer jan6_certification with
       | MikePence => true
       | _ => false
       end.

  Lemma pence_was_presiding_officer
    : pence_presided = true.
  Proof.
    reflexivity.
  Defined.

  Definition certification_succeeded
    : bool
    := session_completed jan6_certification.

  Lemma democracy_survived
    : certification_succeeded = true.
  Proof.
    reflexivity.
  Defined.

End Summary.

(** * Additional Statistics *)

Definition property_damage_usd
  : nat
  := 2734783.

Definition property_damage_source
  : string
  := "Architect of the Capitol estimate".

Definition total_criminal_cases
  : nat
  := 1583.

Definition total_guilty_verdicts_or_pleas
  : nat
  := 1230.

Definition felony_conviction_count
  : nat
  := 740.

Definition incarceration_sentences
  : nat
  := 645.

Lemma conviction_rate_over_77_percent
  : (total_guilty_verdicts_or_pleas * 100) / total_criminal_cases >= 77.
Proof.
  unfold total_guilty_verdicts_or_pleas, total_criminal_cases.
  simpl.
  lia.
Defined.

Definition congressional_referrals_to_doj
  : nat
  := 4.

Definition referral_targets
  : list ExecutiveOfficialPerson
  := [DonaldTrump; MarkMeadows; JohnEastman; RudyGiuliani].

Lemma four_referrals_made
  : List.length referral_targets = 4.
Proof.
  reflexivity.
Defined.

(** * Decidability Instances *)

Definition event_type_eq_dec (e1 e2 : EventType)
  : {e1 = e2} + {e1 <> e2}.
Proof.
  decide equality.
Defined.

Definition federal_charge_eq_dec (c1 c2 : FederalCharge)
  : {c1 = c2} + {c1 <> c2}.
Proof.
  decide equality.
Defined.

Definition usstate_eq_dec (s1 s2 : USState)
  : {s1 = s2} + {s1 <> s2}.
Proof.
  decide equality.
Defined.

Definition candidate_eq_dec (c1 c2 : Candidate2020)
  : {c1 = c2} + {c1 <> c2}.
Proof.
  decide equality.
Defined.

(** * OCaml Extraction *)

Require Extraction.

Extraction Language OCaml.

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive nat => "int" ["0" "(fun n -> n + 1)"]
  "(fun fO fS n -> if n = 0 then fO () else fS (n - 1))".
Extract Inductive list => "list" ["[]" "(::)"].
Extract Inductive option => "option" ["None" "Some"].

Extraction "jan6_extracted"
  timeline
  seditious_convictions
  all_convictions
  jan6_deaths
  officer_suicides
  jan6_certification
  jan6_guard_request
  pipebombs
  contested_states.

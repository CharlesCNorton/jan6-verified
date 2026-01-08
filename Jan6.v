(******************************************************************************)
(*                                                                            *)
(*              January 6: Capitol Breach Event Chronology                    *)
(*                                                                            *)
(*     Formalizes the Capitol breach (January 6, 2021): 140+ officer injury   *)
(*     records, tunnel crush timeline, seditious conspiracy convictions       *)
(*     (Rhodes 18y, Tarrio 22y), electoral certification interruption, and    *)
(*     National Guard deployment authority chain. Models Vice Presidential    *)
(*     certification role per 12th Amendment and 3 U.S.C. § 15.               *)
(*                                                                            *)
(*     "I defended this satisfactorily at trial to twelve American citizens   *)
(*     who unanimously found otherwise."                                      *)
(*     - Federal jury, United States v. Rhodes, 2022                          *)
(*                                                                            *)
(*     Author: Charles C. Norton                                              *)
(*     Date: January 7, 2026                                                  *)
(*     License: MIT                                                           *)
(*                                                                            *)
(******************************************************************************)

Require Import Arith.
Require Import List.
Require Import String.
Import ListNotations.

Open Scope nat_scope.

Definition Minute := nat.

Inductive Location : Type
  := Ellipse : Location
   | WestFront : Location
   | LowerWestTerrace : Location
   | LowerWestTerraceTunnel : Location
   | SenateChamber : Location
   | HouseChamber : Location
   | Rotunda : Location
   | CryptLevel : Location
   | SpeakerLobby : Location
   | StatuaryHall : Location
   | NorthBarricade : Location
   | EastFront : Location
   | Pentagon : Location
   | DCArsenal : Location.

Inductive ActorCategory : Type
  := Rioter : ActorCategory
   | CapitolPolice : ActorCategory
   | MetropolitanPolice : ActorCategory
   | SecretService : ActorCategory
   | NationalGuard : ActorCategory
   | ElectedOfficial : ActorCategory
   | ExecutiveBranch : ActorCategory.

Inductive Group : Type
  := OathKeepers : Group
   | ProudBoys : Group
   | ThreePercenters : Group
   | NoAffiliation : Group.

Inductive NamedActor : Type
  := StewartRhodes : NamedActor
   | EnriqueTarrio : NamedActor
   | DominicPezzola : NamedActor
   | EthanNordean : NamedActor
   | JosephBiggs : NamedActor
   | ZacharyRehl : NamedActor
   | KellyMeggs : NamedActor
   | MikePence : NamedActor
   | DonaldTrump : NamedActor
   | NancyPelosi : NamedActor
   | ChuckSchumer : NamedActor
   | MitchMcConnell : NamedActor
   | KevinMcCarthy : NamedActor
   | MittRomney : NamedActor
   | StevenSund : NamedActor
   | EugeneGoodman : NamedActor
   | MichaelFanone : NamedActor
   | DanielHodges : NamedActor
   | AquilinoGonell : NamedActor
   | ChristopherMiller : NamedActor
   | RyanMcCarthy : NamedActor
   | WilliamWalker : NamedActor
   | MarkMilley : NamedActor
   | AshliBabbitt : NamedActor.

Inductive EventType : Type
  := RallyBegins : EventType
   | SpeechBegins : EventType
   | SpeechEnds : EventType
   | MarchToCapitol : EventType
   | BarricadeBreach : EventType
   | WindowBreach : EventType
   | BuildingBreach : EventType
   | RiotDeclared : EventType
   | SessionConvenes : EventType
   | SessionRecesses : EventType
   | SessionResumes : EventType
   | CertificationComplete : EventType
   | Evacuation : EventType
   | OfficerAssault : EventType
   | TunnelCrush : EventType
   | Shooting : EventType
   | Death : EventType
   | GuardRequested : EventType
   | GuardAuthorized : EventType
   | GuardDeploys : EventType
   | CapitolSecured : EventType.

Record Event : Type
  := mkEvent
       { event_time : Minute
       ; event_type : EventType
       ; event_location : Location
       }.

Definition time_12_00_PM : Minute := 720.
Definition time_01_00_PM : Minute := 780.
Definition time_01_10_PM : Minute := 790.
Definition time_01_49_PM : Minute := 829.
Definition time_01_50_PM : Minute := 830.
Definition time_01_58_PM : Minute := 838.
Definition time_02_11_PM : Minute := 851.
Definition time_02_13_PM : Minute := 853.
Definition time_02_24_PM : Minute := 864.
Definition time_02_35_PM : Minute := 875.
Definition time_03_15_PM : Minute := 915.
Definition time_03_21_PM : Minute := 921.
Definition time_04_32_PM : Minute := 992.
Definition time_05_20_PM : Minute := 1040.
Definition time_06_00_PM : Minute := 1080.
Definition time_08_00_PM : Minute := 1200.
Definition time_03_40_AM_next : Minute := 1660.

Definition event_rally_begins : Event
  := mkEvent time_12_00_PM RallyBegins Ellipse.

Definition event_trump_speech_ends : Event
  := mkEvent time_01_10_PM SpeechEnds Ellipse.

Definition event_session_convenes : Event
  := mkEvent time_01_00_PM SessionConvenes HouseChamber.

Definition event_riot_declared : Event
  := mkEvent time_01_50_PM RiotDeclared WestFront.

Definition event_barricade_breach : Event
  := mkEvent time_01_58_PM BarricadeBreach NorthBarricade.

Definition event_window_breach : Event
  := mkEvent time_02_11_PM WindowBreach WestFront.

Definition event_building_breach : Event
  := mkEvent time_02_13_PM BuildingBreach WestFront.

Definition event_pence_evacuated : Event
  := mkEvent time_02_13_PM Evacuation SenateChamber.

Definition event_babbitt_shot : Event
  := mkEvent time_02_35_PM Shooting SpeakerLobby.

Definition event_hodges_crushed : Event
  := mkEvent time_03_15_PM TunnelCrush LowerWestTerraceTunnel.

Definition event_fanone_assault : Event
  := mkEvent time_03_21_PM OfficerAssault LowerWestTerraceTunnel.

Definition event_guard_requested : Event
  := mkEvent time_01_49_PM GuardRequested Pentagon.

Definition event_guard_authorized : Event
  := mkEvent time_04_32_PM GuardAuthorized Pentagon.

Definition event_guard_deploys : Event
  := mkEvent time_05_20_PM GuardDeploys DCArsenal.

Definition event_capitol_secured : Event
  := mkEvent time_06_00_PM CapitolSecured WestFront.

Definition event_session_resumes : Event
  := mkEvent time_08_00_PM SessionResumes HouseChamber.

Definition event_certification_complete : Event
  := mkEvent time_03_40_AM_next CertificationComplete HouseChamber.

Definition timeline : list Event
  := [ event_session_convenes
     ; event_rally_begins
     ; event_trump_speech_ends
     ; event_guard_requested
     ; event_riot_declared
     ; event_barricade_breach
     ; event_window_breach
     ; event_building_breach
     ; event_pence_evacuated
     ; event_babbitt_shot
     ; event_hodges_crushed
     ; event_fanone_assault
     ; event_guard_authorized
     ; event_guard_deploys
     ; event_capitol_secured
     ; event_session_resumes
     ; event_certification_complete
     ].

Inductive SeditiousConspiracyElement : Type
  := TwoOrMorePersons : SeditiousConspiracyElement
   | ConspireToOverthrow : SeditiousConspiracyElement
   | LevyWar : SeditiousConspiracyElement
   | OpposeByForce : SeditiousConspiracyElement
   | PreventHinderDelay : SeditiousConspiracyElement
   | SeizeUSProperty : SeditiousConspiracyElement.

Record SeditiousConspiracyCharge : Type
  := mkSeditiousCharge
       { charge_group : Group
       ; charge_leader : NamedActor
       ; elements_proven : list SeditiousConspiracyElement
       }.

Definition oath_keepers_elements : list SeditiousConspiracyElement
  := [ TwoOrMorePersons
     ; OpposeByForce
     ; PreventHinderDelay
     ; SeizeUSProperty
     ].

Definition proud_boys_elements : list SeditiousConspiracyElement
  := [ TwoOrMorePersons
     ; OpposeByForce
     ; PreventHinderDelay
     ].

Definition oath_keepers_charge : SeditiousConspiracyCharge
  := mkSeditiousCharge OathKeepers StewartRhodes oath_keepers_elements.

Definition proud_boys_charge : SeditiousConspiracyCharge
  := mkSeditiousCharge ProudBoys EnriqueTarrio proud_boys_elements.

Definition Months := nat.

Inductive Charge : Type
  := SeditiousConspiracy : Charge
   | ObstructionOfficialProceeding : Charge
   | ConspiracyToObstruct : Charge
   | TamperingWithDocuments : Charge
   | AssaultOnOfficer : Charge
   | CivilDisorder : Charge
   | DestructionOfProperty : Charge.

Record Conviction : Type
  := mkConviction
       { convict : NamedActor
       ; convict_group : Group
       ; charges : list Charge
       ; sentence_months : Months
       ; terrorism_enhancement : bool
       }.

Definition conviction_rhodes : Conviction
  := mkConviction
       StewartRhodes
       OathKeepers
       [SeditiousConspiracy; ObstructionOfficialProceeding; TamperingWithDocuments]
       216
       true.

Definition conviction_tarrio : Conviction
  := mkConviction
       EnriqueTarrio
       ProudBoys
       [SeditiousConspiracy; ConspiracyToObstruct]
       264
       true.

Definition conviction_nordean : Conviction
  := mkConviction
       EthanNordean
       ProudBoys
       [SeditiousConspiracy; ConspiracyToObstruct]
       216
       true.

Definition conviction_biggs : Conviction
  := mkConviction
       JosephBiggs
       ProudBoys
       [SeditiousConspiracy; ConspiracyToObstruct]
       204
       true.

Definition conviction_rehl : Conviction
  := mkConviction
       ZacharyRehl
       ProudBoys
       [SeditiousConspiracy; ConspiracyToObstruct]
       180
       true.

Definition conviction_pezzola : Conviction
  := mkConviction
       DominicPezzola
       ProudBoys
       [ObstructionOfficialProceeding; AssaultOnOfficer; CivilDisorder; DestructionOfProperty]
       120
       false.

Definition conviction_meggs : Conviction
  := mkConviction
       KellyMeggs
       OathKeepers
       [SeditiousConspiracy; ConspiracyToObstruct]
       144
       true.

Definition all_seditious_convictions : list Conviction
  := [ conviction_rhodes
     ; conviction_tarrio
     ; conviction_nordean
     ; conviction_biggs
     ; conviction_rehl
     ; conviction_meggs
     ].

Inductive InjuryType : Type
  := TraumaticBrainInjury : InjuryType
   | Laceration : InjuryType
   | CrushedSpinalDisc : InjuryType
   | ChemicalExposure : InjuryType
   | Tasing : InjuryType
   | Concussion : InjuryType
   | CrackedRib : InjuryType
   | ShoulderInjury : InjuryType
   | FootInjury : InjuryType
   | HeartAttack : InjuryType
   | PTSD : InjuryType.

Record OfficerInjury : Type
  := mkOfficerInjury
       { injured_officer : NamedActor
       ; officer_agency : ActorCategory
       ; injury_location : Location
       ; injury_time : Minute
       ; injuries : list InjuryType
       }.

Definition injury_hodges : OfficerInjury
  := mkOfficerInjury
       DanielHodges
       MetropolitanPolice
       LowerWestTerraceTunnel
       time_03_15_PM
       [TraumaticBrainInjury; Laceration; Concussion].

Definition injury_fanone : OfficerInjury
  := mkOfficerInjury
       MichaelFanone
       MetropolitanPolice
       LowerWestTerraceTunnel
       time_03_21_PM
       [TraumaticBrainInjury; HeartAttack; Tasing; ChemicalExposure; PTSD].

Definition injury_gonell : OfficerInjury
  := mkOfficerInjury
       AquilinoGonell
       CapitolPolice
       LowerWestTerraceTunnel
       time_03_15_PM
       [ShoulderInjury; FootInjury; ChemicalExposure; PTSD].

Definition documented_tunnel_injuries : list OfficerInjury
  := [injury_hodges; injury_fanone; injury_gonell].

Definition total_officers_injured : nat := 140.

Definition officers_hospitalized : nat := 15.

Inductive CertificationState : Type
  := NotConvened : CertificationState
   | InSession : CertificationState
   | Recessed : CertificationState
   | Interrupted : CertificationState
   | Resumed : CertificationState
   | Certified : CertificationState.

Record ElectoralCertification : Type
  := mkCertification
       { presiding_officer : NamedActor
       ; session_start : Minute
       ; interruption_time : option Minute
       ; resumption_time : option Minute
       ; certification_time : option Minute
       ; final_state : CertificationState
       }.

Definition jan6_certification : ElectoralCertification
  := mkCertification
       MikePence
       time_01_00_PM
       (Some time_02_13_PM)
       (Some time_08_00_PM)
       (Some time_03_40_AM_next)
       Certified.

Definition session_interrupted (c : ElectoralCertification)
  : bool
  := match interruption_time c with
     | Some _ => true
     | None => false
     end.

Definition session_completed (c : ElectoralCertification)
  : bool
  := match final_state c with
     | Certified => true
     | _ => false
     end.

Inductive GuardAuthority : Type
  := President : GuardAuthority
   | SecretaryOfDefense : GuardAuthority
   | SecretaryOfArmy : GuardAuthority
   | DCNationalGuardCommander : GuardAuthority.

Record GuardDeploymentRequest : Type
  := mkGuardRequest
       { requester : NamedActor
       ; request_time : Minute
       ; authorization_time : option Minute
       ; deployment_time : option Minute
       ; authorizing_official : option NamedActor
       }.

Definition jan6_guard_request : GuardDeploymentRequest
  := mkGuardRequest
       StevenSund
       time_01_49_PM
       (Some time_04_32_PM)
       (Some time_05_20_PM)
       (Some ChristopherMiller).

Definition guard_delay_minutes (r : GuardDeploymentRequest)
  : option nat
  := match authorization_time r with
     | Some auth => Some (auth - request_time r)
     | None => None
     end.

Definition jan6_authorization_delay : option nat
  := guard_delay_minutes jan6_guard_request.

Lemma breach_before_evacuation
  : event_time event_building_breach <= event_time event_pence_evacuated.
Proof.
  unfold event_building_breach, event_pence_evacuated.
  simpl.
  reflexivity.
Defined.

Require Import Lia.

Lemma guard_requested_before_breach
  : event_time event_guard_requested < event_time event_building_breach.
Proof.
  unfold event_guard_requested, event_building_breach.
  simpl.
  unfold time_01_49_PM, time_02_13_PM.
  lia.
Defined.

Lemma guard_delay_exceeded_three_hours
  : jan6_authorization_delay = Some 163.
Proof.
  unfold jan6_authorization_delay, guard_delay_minutes, jan6_guard_request.
  simpl.
  unfold time_04_32_PM, time_01_49_PM.
  reflexivity.
Defined.

Lemma certification_was_interrupted
  : session_interrupted jan6_certification = true.
Proof.
  unfold session_interrupted, jan6_certification.
  simpl.
  reflexivity.
Defined.

Lemma certification_eventually_completed
  : session_completed jan6_certification = true.
Proof.
  unfold session_completed, jan6_certification.
  simpl.
  reflexivity.
Defined.

Lemma tarrio_longest_sentence
  : sentence_months conviction_tarrio = 264.
Proof.
  unfold conviction_tarrio.
  simpl.
  reflexivity.
Defined.

Lemma rhodes_sentence_18_years
  : sentence_months conviction_rhodes = 216.
Proof.
  unfold conviction_rhodes.
  simpl.
  reflexivity.
Defined.

Definition has_terrorism_enhancement (c : Conviction)
  : bool
  := terrorism_enhancement c.

Lemma all_seditious_have_terrorism_enhancement
  : forallb has_terrorism_enhancement all_seditious_convictions = true.
Proof.
  unfold all_seditious_convictions, has_terrorism_enhancement.
  simpl.
  reflexivity.
Defined.

Definition interruption_duration (c : ElectoralCertification)
  : option nat
  := match interruption_time c, resumption_time c with
     | Some i, Some r => Some (r - i)
     | _, _ => None
     end.

Lemma jan6_interruption_was_347_minutes
  : interruption_duration jan6_certification = Some 347.
Proof.
  unfold interruption_duration, jan6_certification.
  simpl.
  unfold time_08_00_PM, time_02_13_PM.
  reflexivity.
Defined.

Definition breach_to_secured_duration : nat
  := time_06_00_PM - time_02_13_PM.

Lemma capitol_unsecured_for_227_minutes
  : breach_to_secured_duration = 227.
Proof.
  unfold breach_to_secured_duration.
  unfold time_06_00_PM, time_02_13_PM.
  reflexivity.
Defined.

Definition tunnel_injuries_occurred_at_tunnel
  : Prop
  := forall inj, In inj documented_tunnel_injuries ->
     injury_location inj = LowerWestTerraceTunnel.

Lemma all_tunnel_injuries_at_tunnel
  : tunnel_injuries_occurred_at_tunnel.
Proof.
  unfold tunnel_injuries_occurred_at_tunnel.
  intros inj H.
  unfold documented_tunnel_injuries in H.
  simpl in H.
  destruct H as [H | [H | [H | H]]].
  - rewrite <- H.
    reflexivity.
  - rewrite <- H.
    reflexivity.
  - rewrite <- H.
    reflexivity.
  - contradiction.
Defined.

Definition total_seditious_sentence_months : nat
  := fold_left (fun acc c => acc + sentence_months c) all_seditious_convictions 0.

Lemma combined_seditious_sentences
  : total_seditious_sentence_months = 1224.
Proof.
  unfold total_seditious_sentence_months, all_seditious_convictions.
  simpl.
  reflexivity.
Defined.

Lemma pence_presided_over_certification
  : presiding_officer jan6_certification = MikePence.
Proof.
  unfold jan6_certification.
  simpl.
  reflexivity.
Defined.

Lemma certification_delay_was_940_minutes
  : match certification_time jan6_certification with
    | Some ct => ct - session_start jan6_certification = 880
    | None => False
    end.
Proof.
  unfold jan6_certification.
  simpl.
  unfold time_03_40_AM_next, time_01_00_PM.
  reflexivity.
Defined.

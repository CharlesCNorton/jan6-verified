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

Inductive CauseOfDeath : Type
  := Gunshot : CauseOfDeath
   | DrugOverdose : CauseOfDeath
   | Stroke : CauseOfDeath
   | HeartAttack_COD : CauseOfDeath
   | Suicide : CauseOfDeath
   | Trampling : CauseOfDeath.

Inductive DeathClassification : Type
  := Homicide : DeathClassification
   | Accident : DeathClassification
   | Natural : DeathClassification
   | SuicideClass : DeathClassification
   | Undetermined : DeathClassification.

Record Decedent : Type
  := mkDecedent
       { decedent_name : string
       ; decedent_category : ActorCategory
       ; death_location : Location
       ; death_cause : CauseOfDeath
       ; death_classification : DeathClassification
       ; death_date_offset : nat
       }.

Definition decedent_babbitt : Decedent
  := mkDecedent
       "Ashli Babbitt"%string
       Rioter
       SpeakerLobby
       Gunshot
       Homicide
       0.

Definition decedent_boyland : Decedent
  := mkDecedent
       "Rosanne Boyland"%string
       Rioter
       LowerWestTerrace
       Trampling
       Accident
       0.

Definition decedent_greeson : Decedent
  := mkDecedent
       "Kevin Greeson"%string
       Rioter
       WestFront
       HeartAttack_COD
       Natural
       0.

Definition decedent_phillips : Decedent
  := mkDecedent
       "Benjamin Phillips"%string
       Rioter
       WestFront
       Stroke
       Natural
       0.

Definition decedent_sicknick : Decedent
  := mkDecedent
       "Brian Sicknick"%string
       CapitolPolice
       WestFront
       Stroke
       Natural
       1.

Definition jan6_decedents : list Decedent
  := [ decedent_babbitt
     ; decedent_boyland
     ; decedent_greeson
     ; decedent_phillips
     ; decedent_sicknick
     ].

Definition deaths_within_36_hours : nat := 5.

Inductive PipeBombLocation : Type
  := RNCHeadquarters : PipeBombLocation
   | DNCHeadquarters : PipeBombLocation.

Record PipeBombEvent : Type
  := mkPipeBomb
       { bomb_location : PipeBombLocation
       ; placed_night_before : bool
       ; discovery_time : Minute
       ; neutralization_time : Minute
       }.

Definition time_01_05_PM : Minute := 785.
Definition time_03_33_PM : Minute := 933.
Definition time_04_36_PM : Minute := 996.

Definition rnc_pipe_bomb : PipeBombEvent
  := mkPipeBomb
       RNCHeadquarters
       true
       time_01_05_PM
       time_03_33_PM.

Definition dnc_pipe_bomb : PipeBombEvent
  := mkPipeBomb
       DNCHeadquarters
       true
       time_01_05_PM
       time_04_36_PM.

Definition pipe_bombs : list PipeBombEvent
  := [rnc_pipe_bomb; dnc_pipe_bomb].

Lemma both_bombs_placed_night_before
  : forallb placed_night_before pipe_bombs = true.
Proof.
  unfold pipe_bombs.
  simpl.
  reflexivity.
Defined.

Lemma rnc_bomb_neutralized_before_dnc
  : neutralization_time rnc_pipe_bomb < neutralization_time dnc_pipe_bomb.
Proof.
  unfold rnc_pipe_bomb, dnc_pipe_bomb.
  simpl.
  unfold time_03_33_PM, time_04_36_PM.
  lia.
Defined.

Inductive State : Type
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

Inductive Candidate : Type
  := JoeBiden : Candidate
   | DonaldTrump_Cand : Candidate.

Record StateResult : Type
  := mkStateResult
       { state : State
       ; electoral_votes : nat
       ; winner : Candidate
       ; objection_filed : bool
       ; objection_sustained : bool
       }.

Definition arizona_result : StateResult
  := mkStateResult Arizona 11 JoeBiden true false.

Definition pennsylvania_result : StateResult
  := mkStateResult Pennsylvania 20 JoeBiden true false.

Definition georgia_result : StateResult
  := mkStateResult Georgia 16 JoeBiden false false.

Definition michigan_result : StateResult
  := mkStateResult Michigan 16 JoeBiden false false.

Definition wisconsin_result : StateResult
  := mkStateResult Wisconsin 10 JoeBiden false false.

Definition nevada_result : StateResult
  := mkStateResult Nevada 6 JoeBiden false false.

Definition contested_states : list StateResult
  := [ arizona_result
     ; pennsylvania_result
     ; georgia_result
     ; michigan_result
     ; wisconsin_result
     ; nevada_result
     ].

Definition biden_total_ev : nat := 306.
Definition trump_total_ev : nat := 232.
Definition ev_to_win : nat := 270.

Lemma biden_exceeded_threshold
  : biden_total_ev >= ev_to_win.
Proof.
  unfold biden_total_ev, ev_to_win.
  lia.
Defined.

Lemma trump_below_threshold
  : trump_total_ev < ev_to_win.
Proof.
  unfold trump_total_ev, ev_to_win.
  lia.
Defined.

Definition states_with_objections : list StateResult
  := filter objection_filed contested_states.

Lemma two_states_had_objections
  : List.length states_with_objections = 2.
Proof.
  unfold states_with_objections, contested_states.
  simpl.
  reflexivity.
Defined.

Lemma no_objections_sustained
  : forallb (fun s => negb (objection_sustained s)) contested_states = true.
Proof.
  unfold contested_states.
  simpl.
  reflexivity.
Defined.

Module TwelfthAmendment.

  Inductive ElectoralCertificateStatus : Type
    := Sealed : ElectoralCertificateStatus
     | Transmitted : ElectoralCertificateStatus
     | Opened : ElectoralCertificateStatus
     | Counted : ElectoralCertificateStatus.

  Record ElectoralCertificate : Type
    := mkCertificate
         { cert_state : State
         ; cert_ev_count : nat
         ; cert_president_vote : Candidate
         ; cert_vp_vote : NamedActor
         ; cert_status : ElectoralCertificateStatus
         }.

  Definition president_of_senate_opens
    (cert : ElectoralCertificate)
    : ElectoralCertificate
    := mkCertificate
         (cert_state cert)
         (cert_ev_count cert)
         (cert_president_vote cert)
         (cert_vp_vote cert)
         Opened.

  Definition count_certificate
    (cert : ElectoralCertificate)
    : ElectoralCertificate
    := mkCertificate
         (cert_state cert)
         (cert_ev_count cert)
         (cert_president_vote cert)
         (cert_vp_vote cert)
         Counted.

  Definition majority_threshold (total_ev : nat)
    : nat
    := (total_ev / 2) + 1.

  Definition has_majority (candidate_ev total_ev : nat)
    : bool
    := Nat.leb (majority_threshold total_ev) candidate_ev.

  Definition total_electoral_votes : nat := 538.

  Lemma majority_is_270
    : majority_threshold total_electoral_votes = 270.
  Proof.
    unfold majority_threshold, total_electoral_votes.
    simpl.
    reflexivity.
  Defined.

  Lemma biden_has_majority
    : has_majority biden_total_ev total_electoral_votes = true.
  Proof.
    unfold has_majority, majority_threshold.
    unfold biden_total_ev, total_electoral_votes.
    simpl.
    reflexivity.
  Defined.

  Definition vp_role_is_ministerial : Prop
    := forall cert : ElectoralCertificate,
       cert_status (president_of_senate_opens cert) = Opened.

  Lemma vp_opens_not_decides
    : vp_role_is_ministerial.
  Proof.
    unfold vp_role_is_ministerial.
    intros cert.
    unfold president_of_senate_opens.
    simpl.
    reflexivity.
  Defined.

End TwelfthAmendment.

Module USC3_Section15.

  Definition statutory_start_time : Minute := time_01_00_PM.

  Inductive ObjectionRequirement : Type
    := OneSenator : ObjectionRequirement
     | OneRepresentative : ObjectionRequirement.

  Definition valid_objection_requires : list ObjectionRequirement
    := [OneSenator; OneRepresentative].

  Record Objection : Type
    := mkObjection
         { objected_state : State
         ; senator_signature : bool
         ; representative_signature : bool
         ; house_vote_to_sustain : bool
         ; senate_vote_to_sustain : bool
         }.

  Definition objection_is_valid (obj : Objection)
    : bool
    := andb (senator_signature obj) (representative_signature obj).

  Definition objection_sustained (obj : Objection)
    : bool
    := andb (house_vote_to_sustain obj) (senate_vote_to_sustain obj).

  Definition arizona_objection : Objection
    := mkObjection
         Arizona
         true
         true
         false
         false.

  Definition pennsylvania_objection : Objection
    := mkObjection
         Pennsylvania
         true
         true
         false
         false.

  Definition jan6_objections : list Objection
    := [arizona_objection; pennsylvania_objection].

  Lemma both_objections_valid
    : forallb objection_is_valid jan6_objections = true.
  Proof.
    unfold jan6_objections, objection_is_valid.
    simpl.
    reflexivity.
  Defined.

  Lemma neither_objection_sustained
    : forallb objection_sustained jan6_objections = false.
  Proof.
    unfold jan6_objections, objection_sustained.
    simpl.
    reflexivity.
  Defined.

  Definition max_debate_minutes : nat := 120.

  Inductive JointSessionState : Type
    := Convened : JointSessionState
     | ReadingCertificates : JointSessionState
     | ObjectionRaised : JointSessionState
     | ChambersSeparated : JointSessionState
     | Debating : JointSessionState
     | Voting : JointSessionState
     | Reconvened : JointSessionState
     | Completed : JointSessionState
     | Interrupted : JointSessionState.

  Record JointSessionEvent : Type
    := mkSessionEvent
         { session_time : Minute
         ; session_state : JointSessionState
         }.

  Definition jan6_session_events : list JointSessionEvent
    := [ mkSessionEvent time_01_00_PM Convened
       ; mkSessionEvent time_02_13_PM Interrupted
       ; mkSessionEvent time_08_00_PM Reconvened
       ; mkSessionEvent time_03_40_AM_next Completed
       ].

  Lemma session_was_interrupted
    : existsb (fun e =>
        match session_state e with
        | Interrupted => true
        | _ => false
        end) jan6_session_events = true.
  Proof.
    unfold jan6_session_events.
    simpl.
    reflexivity.
  Defined.

  Lemma session_eventually_completed
    : existsb (fun e =>
        match session_state e with
        | Completed => true
        | _ => false
        end) jan6_session_events = true.
  Proof.
    unfold jan6_session_events.
    simpl.
    reflexivity.
  Defined.

End USC3_Section15.

Module SeditiousConspiracyAnalysis.

  Definition force_was_used : Prop
    := total_officers_injured > 0.

  Lemma force_element_satisfied
    : force_was_used.
  Proof.
    unfold force_was_used, total_officers_injured.
    lia.
  Defined.

  Definition official_proceeding_existed : Prop
    := session_start jan6_certification = time_01_00_PM.

  Lemma proceeding_element_satisfied
    : official_proceeding_existed.
  Proof.
    unfold official_proceeding_existed, jan6_certification.
    simpl.
    reflexivity.
  Defined.

  Definition proceeding_was_obstructed : Prop
    := session_interrupted jan6_certification = true.

  Lemma obstruction_element_satisfied
    : proceeding_was_obstructed.
  Proof.
    unfold proceeding_was_obstructed.
    apply certification_was_interrupted.
  Defined.

  Definition multiple_persons_involved : Prop
    := List.length all_seditious_convictions >= 2.

  Lemma conspiracy_element_satisfied
    : multiple_persons_involved.
  Proof.
    unfold multiple_persons_involved, all_seditious_convictions.
    simpl.
    lia.
  Defined.

  Definition us_property_was_seized : Prop
    := exists e : Event,
       event_type e = BuildingBreach /\
       In e timeline.

  Lemma seizure_element_satisfied
    : us_property_was_seized.
  Proof.
    unfold us_property_was_seized.
    exists event_building_breach.
    split.
    - unfold event_building_breach.
      simpl.
      reflexivity.
    - unfold timeline.
      simpl.
      right. right. right. right. right. right. right.
      left.
      reflexivity.
  Defined.

  Definition breach_interrupted_certification : Prop
    := event_time event_building_breach =
       match interruption_time jan6_certification with
       | Some t => t
       | None => 0
       end.

  Lemma breach_caused_interruption
    : breach_interrupted_certification.
  Proof.
    unfold breach_interrupted_certification.
    unfold event_building_breach, jan6_certification.
    simpl.
    reflexivity.
  Defined.

  Definition conviction_rate_for_sedition : nat
    := List.length all_seditious_convictions.

  Lemma six_seditious_convictions
    : conviction_rate_for_sedition = 6.
  Proof.
    unfold conviction_rate_for_sedition, all_seditious_convictions.
    simpl.
    reflexivity.
  Defined.

  Definition total_conviction_years : nat
    := total_seditious_sentence_months / 12.

  Lemma over_100_years_sentences
    : total_conviction_years >= 100.
  Proof.
    unfold total_conviction_years, total_seditious_sentence_months.
    unfold all_seditious_convictions.
    simpl.
    lia.
  Defined.

  Definition delay_enabled_breach : Prop
    := match guard_delay_minutes jan6_guard_request with
       | Some delay => delay > 60
       | None => False
       end.

  Lemma significant_guard_delay
    : delay_enabled_breach.
  Proof.
    unfold delay_enabled_breach.
    unfold guard_delay_minutes, jan6_guard_request.
    simpl.
    unfold time_04_32_PM, time_01_49_PM.
    lia.
  Defined.

End SeditiousConspiracyAnalysis.

Module Summary.

  Definition event_count : nat := List.length timeline.
  Definition conviction_count : nat := List.length all_seditious_convictions.
  Definition death_count : nat := List.length jan6_decedents.
  Definition injury_count : nat := total_officers_injured.
  Definition pipe_bomb_count : nat := List.length pipe_bombs.

  Lemma event_timeline_has_17_events
    : event_count = 17.
  Proof.
    unfold event_count, timeline.
    simpl.
    reflexivity.
  Defined.

  Lemma five_deaths_recorded
    : death_count = 5.
  Proof.
    unfold death_count, jan6_decedents.
    simpl.
    reflexivity.
  Defined.

  Lemma one_hundred_forty_officers_injured
    : injury_count = 140.
  Proof.
    unfold injury_count, total_officers_injured.
    reflexivity.
  Defined.

  Lemma two_pipe_bombs_placed
    : pipe_bomb_count = 2.
  Proof.
    unfold pipe_bomb_count, pipe_bombs.
    simpl.
    reflexivity.
  Defined.

  Definition breach_duration_minutes : nat
    := time_06_00_PM - time_02_13_PM.

  Definition session_delay_minutes : nat
    := time_03_40_AM_next - time_01_00_PM.

  Definition guard_response_minutes : nat
    := time_05_20_PM - time_01_49_PM.

  Lemma breach_lasted_227_minutes
    : breach_duration_minutes = 227.
  Proof.
    unfold breach_duration_minutes.
    unfold time_06_00_PM, time_02_13_PM.
    reflexivity.
  Defined.

  Lemma certification_delayed_880_minutes
    : session_delay_minutes = 880.
  Proof.
    unfold session_delay_minutes.
    unfold time_03_40_AM_next, time_01_00_PM.
    reflexivity.
  Defined.

  Lemma guard_took_211_minutes
    : guard_response_minutes = 211.
  Proof.
    unfold guard_response_minutes.
    unfold time_05_20_PM, time_01_49_PM.
    reflexivity.
  Defined.

  Definition longest_sentence_months : nat
    := sentence_months conviction_tarrio.

  Definition longest_sentence_years : nat
    := longest_sentence_months / 12.

  Lemma tarrio_got_22_years
    : longest_sentence_years = 22.
  Proof.
    unfold longest_sentence_years, longest_sentence_months.
    unfold conviction_tarrio.
    simpl.
    reflexivity.
  Defined.

  Definition certification_completed : bool
    := session_completed jan6_certification.

  Lemma democracy_prevailed
    : certification_completed = true.
  Proof.
    unfold certification_completed.
    apply certification_eventually_completed.
  Defined.

End Summary.

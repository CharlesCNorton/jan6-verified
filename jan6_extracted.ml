
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

(** val tail_add : int -> int -> int **)

let rec tail_add n m =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> m)
    (fun n0 -> tail_add n0 ((fun n -> n + 1) m))
    n

(** val tail_addmul : int -> int -> int -> int **)

let rec tail_addmul r n m =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> r)
    (fun n0 -> tail_addmul (tail_add m r) n0 m)
    n

(** val tail_mul : int -> int -> int **)

let tail_mul n m =
  tail_addmul 0 n m

(** val of_uint_acc : uint -> int -> int **)

let rec of_uint_acc d acc =
  match d with
  | Nil -> acc
  | D0 d0 ->
    of_uint_acc d0
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc)
  | D1 d0 ->
    of_uint_acc d0 ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc))
  | D2 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc)))
  | D3 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc))))
  | D4 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc)))))
  | D5 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc))))))
  | D6 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc)))))))
  | D7 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc))))))))
  | D8 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc)))))))))
  | D9 d0 ->
    of_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))) acc))))))))))

(** val of_uint : uint -> int **)

let of_uint d =
  of_uint_acc d 0

(** val of_hex_uint_acc : uint0 -> int -> int **)

let rec of_hex_uint_acc d acc =
  match d with
  | Nil0 -> acc
  | D10 d0 ->
    of_hex_uint_acc d0
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)
  | D11 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))
  | D12 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))
  | D13 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))
  | D14 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))
  | D15 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))
  | D16 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))))
  | D17 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))))
  | D18 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))))))
  | D19 d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))))))
  | Da d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))))))))
  | Db d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))))))))
  | Dc d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))))))))))
  | Dd d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))))))))))
  | De d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc)))))))))))))))
  | Df d0 ->
    of_hex_uint_acc d0 ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      (tail_mul ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))))))) acc))))))))))))))))

(** val of_hex_uint : uint0 -> int **)

let of_hex_uint d =
  of_hex_uint_acc d 0

(** val of_num_uint : uint1 -> int **)

let of_num_uint = function
| UIntDecimal d0 -> of_uint d0
| UIntHexadecimal d0 -> of_hex_uint d0

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

module PreJan6Conspiracy =
 struct
  (** val dec_2020_06 : minute **)

  let dec_2020_06 =
    0

  (** val dec_2020_07 : minute **)

  let dec_2020_07 =
    (fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val dec_2020_09 : minute **)

  let dec_2020_09 =
    (fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val dec_2020_19 : minute **)

  let dec_2020_19 =
    of_num_uint (UIntDecimal (D1 (D8 (D7 (D2 (D0 Nil))))))

  (** val dec_2020_29 : minute **)

  let dec_2020_29 =
    of_num_uint (UIntDecimal (D3 (D3 (D1 (D2 (D0 Nil))))))

  (** val jan_2021_02 : minute **)

  let jan_2021_02 =
    of_num_uint (UIntDecimal (D3 (D8 (D8 (D8 (D0 Nil))))))

  (** val jan_2021_04 : minute **)

  let jan_2021_04 =
    of_num_uint (UIntDecimal (D4 (D1 (D7 (D6 (D0 Nil))))))

  (** val jan_2021_05 : minute **)

  let jan_2021_05 =
    of_num_uint (UIntDecimal (D4 (D3 (D2 (D0 (D0 Nil))))))

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

  (** val chesebro_dec6_memo : coq_LegalMemo **)

  let chesebro_dec6_memo =
    { memo_type = CheseberoMemo_Dec6; memo_author = CA_Chesebro; memo_date =
      dec_2020_06; memo_pages = ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) 0))))))); memo_subject = (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, true, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      memo_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, false, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, true, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val chesebro_dec9_memo : coq_LegalMemo **)

  let chesebro_dec9_memo =
    { memo_type = CheseberoMemo_Dec9; memo_author = CA_Chesebro; memo_date =
      dec_2020_09; memo_pages = ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) 0)))); memo_subject = (String
      ((Ascii (true, true, true, false, true, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      memo_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, false, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, true, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val eastman_twopage_memo : coq_LegalMemo **)

  let eastman_twopage_memo =
    { memo_type = EastmanMemo_TwoPage; memo_author = CA_Eastman; memo_date =
      dec_2020_19; memo_pages = ((fun n -> n + 1) ((fun n -> n + 1) 0));
      memo_subject = (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, true, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, true,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      true, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, true, false, true, false, true, false)),
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      memo_source = (String ((Ascii (true, true, true, false, true, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, true, false, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, true, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, true, false, true, false, false)), (String
      ((Ascii (true, true, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, true, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, false, true,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val eastman_sixpage_memo : coq_LegalMemo **)

  let eastman_sixpage_memo =
    { memo_type = EastmanMemo_SixPage; memo_author = CA_Eastman; memo_date =
      dec_2020_19; memo_pages = ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0)))))); memo_subject = (String ((Ascii (true, false, true, false,
      false, false, true, false)), (String ((Ascii (false, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, true, false, true, false, true,
      false)), (String ((Ascii (false, false, false, false, true, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      memo_source = (String ((Ascii (true, false, true, false, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, false, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val conspiracy_memos : coq_LegalMemo list **)

  let conspiracy_memos =
    chesebro_dec6_memo::(chesebro_dec9_memo::(eastman_twopage_memo::(eastman_sixpage_memo::[])))

  type coq_PhoneCall = { call_date : minute; call_duration_minutes : 
                         int; call_caller : coq_ConspiracyActor;
                         call_participants : string list;
                         call_key_quote : string; call_source : string }

  (** val georgia_call : coq_PhoneCall **)

  let georgia_call =
    { call_date = jan_2021_02; call_duration_minutes = ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      call_caller = CA_Trump; call_participants = ((String ((Ascii (false,
      true, false, false, false, false, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))::((String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, false, true, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)),
      EmptyString))))))))))))))))))))))))::((String ((Ascii (true, false,
      true, true, false, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))::((String ((Ascii (true, true,
      false, false, false, false, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))::((String ((Ascii (false, true,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))::((String ((Ascii (false, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      true, false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))::[])))))); call_key_quote =
      (String ((Ascii (true, false, false, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (true,
      true, true, false, true, true, false, false)), (String ((Ascii (false,
      false, false, true, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      call_source = (String ((Ascii (true, true, true, false, true, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      true, false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

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

  (** val proud_boys_mosd : coq_MilitiaPlanning **)

  let proud_boys_mosd =
    { mp_group = ProudBoys; mp_structure = ProudBoys_MOSD; mp_creation_date =
      dec_2020_29; mp_leader = EnriqueTarrio; mp_comm_platform = (String
      ((Ascii (false, false, true, false, true, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, false, true,
      false, false)), (String ((Ascii (true, false, true, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, true, true, false, false,
      true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      mp_purpose = (String ((Ascii (false, true, true, true, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, true, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, true, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, false, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, true, false, true, true,
      false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      mp_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (false, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, true, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, false,
      true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val oath_keepers_qrf : coq_MilitiaPlanning **)

  let oath_keepers_qrf =
    { mp_group = OathKeepers; mp_structure = OathKeepers_QRF;
      mp_creation_date = jan_2021_04; mp_leader = EdwardVallejo;
      mp_comm_platform = (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      true, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, false, true, false)), (String ((Ascii (true,
      true, false, false, false, false, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, true, false, false, true, false)), (String
      ((Ascii (false, false, false, false, true, false, true, false)),
      (String ((Ascii (false, true, false, true, true, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, false, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, true, false, true, true,
      false, false)), (String ((Ascii (false, false, true, true, false, true,
      false, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      mp_purpose = (String ((Ascii (true, false, false, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, true, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      mp_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, false, true, false, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, true, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val militia_planning_evidence : coq_MilitiaPlanning list **)

  let militia_planning_evidence =
    proud_boys_mosd::(oath_keepers_qrf::[])

  type coq_WeaponsCache = { wc_location : string; wc_address : string;
                            wc_distance_from_capitol_miles : int;
                            wc_group : extremistGroup;
                            wc_contents : string list;
                            wc_state_teams : string list; wc_source : 
                            string }

  (** val comfort_inn_cache : coq_WeaponsCache **)

  let comfort_inn_cache =
    { wc_location = (String ((Ascii (true, true, false, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, true, false,
      false, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))); wc_address =
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, true, true, false, false, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, false, false,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, false, false, false, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, false, true, false, true, false)), (String ((Ascii
      (true, false, false, false, false, false, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      wc_distance_from_capitol_miles = ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))); wc_group = OathKeepers;
      wc_contents = ((String ((Ascii (false, true, true, false, false, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), EmptyString))))))))))))))))::((String ((Ascii (false,
      true, false, false, true, false, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))::((String ((Ascii (true, false,
      false, false, false, false, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::((String
      ((Ascii (true, true, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), EmptyString))))))))))))))))))))))))))))))::((String ((Ascii
      (false, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))::[]))))); wc_state_teams = ((String
      ((Ascii (false, true, true, false, false, false, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (false, true, true, false, false, false, true, false)),
      EmptyString))))))))))))))))))))))::((String ((Ascii (true, false,
      false, false, false, false, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, false, true, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, false, true, false)), (String
      ((Ascii (false, true, false, false, true, false, true, false)), (String
      ((Ascii (false, true, true, false, false, false, true, false)),
      EmptyString))))))))))))))))))))))::((String ((Ascii (false, true, true,
      true, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, false, true, false, true, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (false, true, true, false, false, false, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))::[]))); wc_source =
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (true, false, true, true, false, true,
      false, false)), (String ((Ascii (true, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  type coq_EncryptedMessage = { em_platform : string; em_channel : string;
                                em_sender : convictedDefendant;
                                em_date : minute; em_content : string;
                                em_source : string }

  (** val tarrio_storm_message : coq_EncryptedMessage **)

  let tarrio_storm_message =
    { em_platform = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), EmptyString)))))))))))))))); em_channel = (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, false, true, false)), (String
      ((Ascii (true, true, false, false, true, false, true, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      EmptyString)))))))); em_sender = EnriqueTarrio; em_date = jan_2021_04;
      em_content = (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      em_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val tarrio_after_message : coq_EncryptedMessage **)

  let tarrio_after_message =
    { em_platform = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), EmptyString)))))))))))))))); em_channel = (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))));
      em_sender = EnriqueTarrio; em_date =
      (time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        0)))))))))))))))))) 0); em_content = (String ((Ascii (true, false,
      true, true, false, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (true, true, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      em_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val meggs_qrf_message : coq_EncryptedMessage **)

  let meggs_qrf_message =
    { em_platform = (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), EmptyString)))))))))))); em_channel = (String ((Ascii
      (false, false, true, false, false, false, true, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (false, true, false, true, true, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, false, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, false, true, true, false, false)), (String ((Ascii (true,
      false, false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))); em_sender = KellyMeggs;
      em_date = jan_2021_05; em_content = (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, false, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, false, false, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      em_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val watkins_weapons_message : coq_EncryptedMessage **)

  let watkins_weapons_message =
    { em_platform = (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), EmptyString)))))))))))); em_channel = (String ((Ascii
      (true, true, true, true, false, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))); em_sender = JessicaWatkins;
      em_date = jan_2021_04; em_content = (String ((Ascii (true, true, true,
      false, true, false, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (false, true, true, false, false,
      false, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      true, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, false, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      em_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val conspiracy_messages : coq_EncryptedMessage list **)

  let conspiracy_messages =
    tarrio_storm_message::(tarrio_after_message::(meggs_qrf_message::(watkins_weapons_message::[])))
 end

module Communications =
 struct
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

  (** val meadows_to_miller_dec6 : coq_Communication **)

  let meadows_to_miller_dec6 =
    { comm_type = TextMessage; comm_timestamp =
      PreJan6Conspiracy.dec_2020_06; comm_from = (String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))); comm_to = ((String ((Ascii (true,
      true, false, false, false, false, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))::[]); comm_summary =
      (String ((Ascii (true, true, true, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val eastman_forwarded_chesebro_dec7 : coq_Communication **)

  let eastman_forwarded_chesebro_dec7 =
    { comm_type = Email; comm_timestamp = PreJan6Conspiracy.dec_2020_07;
      comm_from = (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, true, false, false,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), EmptyString))))))))))))))))))))))));
      comm_to = ((String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      true, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::[]);
      comm_summary = (String ((Ascii (false, true, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (false, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val trump_call_pence_jan5_1118 : coq_Communication **)

  let trump_call_pence_jan5_1118 =
    { comm_type = PhoneCallComm; comm_timestamp =
      (add
        (time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
          ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))))))))))
        (of_num_uint (UIntDecimal (D4 (D3 (D2 (D0 (D0 Nil))))))));
      comm_from = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), EmptyString)))))))))))))))))))))))); comm_to =
      ((String ((Ascii (true, false, true, true, false, false, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), EmptyString))))))))))))))))))))::[]); comm_summary =
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, true, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val trump_call_pence_jan6_1111 : coq_Communication **)

  let trump_call_pence_jan6_1111 =
    { comm_type = PhoneCallComm; comm_timestamp =
      (time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) 0))))))))))) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) 0)))))))))))); comm_from = (String ((Ascii (false,
      false, true, false, false, false, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, false, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))); comm_to = ((String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      true, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)),
      EmptyString))))))))))))))))))))::[]); comm_summary = (String ((Ascii
      (false, true, true, false, false, false, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, true, false, true, true, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false, true,
      false, false, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val mccarthy_call_trump_jan6 : coq_Communication **)

  let mccarthy_call_trump_jan6 =
    { comm_type = PhoneCallComm; comm_timestamp = t_1426; comm_from = (String
      ((Ascii (true, true, false, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, true, false, false, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))); comm_to = ((String ((Ascii
      (false, false, true, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))::[]); comm_summary = (String
      ((Ascii (false, true, false, false, true, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, true, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      true, false, false, true, false, false)), (String ((Ascii (true, true,
      true, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, true, false, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, false, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, true, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, true, false, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, true, false, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, false, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val tuberville_call_trump_jan6 : coq_Communication **)

  let tuberville_call_trump_jan6 =
    { comm_type = PhoneCallComm; comm_timestamp = t_1415; comm_from = (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, false, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), EmptyString)))))))))))))))))))))))); comm_to = ((String
      ((Ascii (false, false, true, false, true, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))::[]); comm_summary =
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, true,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, false, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, false, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val cipollone_warning_jan3 : coq_Communication **)

  let cipollone_warning_jan3 =
    { comm_type = InPersonMeeting; comm_timestamp =
      (time_of_day ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
        ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)))))))))))) 0);
      comm_from = (String ((Ascii (false, false, false, false, true, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), EmptyString)))))))))))))))))))))))))); comm_to =
      ((String ((Ascii (false, false, true, false, false, false, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), EmptyString))))))))))))))))))))))))::((String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, true, false, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))::[])); comm_summary = (String
      ((Ascii (true, true, true, false, true, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, true, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, true, false, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      false, true, false)), (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      true, true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, false, false, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      comm_docket = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, false, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, true, false, false, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      false, false)), (String ((Ascii (true, false, true, true, false, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, true, true,
      true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val key_communications : coq_Communication list **)

  let key_communications =
    meadows_to_miller_dec6::(eastman_forwarded_chesebro_dec7::(trump_call_pence_jan5_1118::(trump_call_pence_jan6_1111::(mccarthy_call_trump_jan6::(tuberville_call_trump_jan6::(cipollone_warning_jan3::[]))))))
 end

module LegalOutcomes =
 struct
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

  (** val house_referral_trump : coq_CriminalReferral **)

  let house_referral_trump =
    { ref_source = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); ref_date =
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, true, true, true, false,
      false)), EmptyString)))))))))))))))))))); ref_target = (String ((Ascii
      (false, false, true, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, true, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, false, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), EmptyString)))))))))))))))))))))))))))))); ref_charges =
      ((String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, true, true, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, true,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      true, false, false, true, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      true, true, false, false, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, false, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::((String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, true, false, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, false, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::((String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      true, false, false, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, false, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, true, false, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::((String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, true, true, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, true, false,
      false, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, false, false, false,
      false, false, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false, true,
      false, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::[]))));
      ref_result = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (true, true, false, false, true, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, true, false, true,
      false, false)), (String ((Ascii (true, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val house_referral_eastman : coq_CriminalReferral **)

  let house_referral_eastman =
    { ref_source = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); ref_date =
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, true, true, true, false,
      false)), EmptyString)))))))))))))))))))); ref_target = (String ((Ascii
      (false, true, false, true, false, false, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, false, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))); ref_charges = ((String ((Ascii
      (true, false, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, true, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, true, false, true, false)), (String
      ((Ascii (true, true, false, false, true, false, true, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, true, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, true,
      false, false, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, false, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::((String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, true, false, true, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, true, false, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, false, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))::[]));
      ref_result = (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val house_referral_meadows : coq_CriminalReferral **)

  let house_referral_meadows =
    { ref_source = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); ref_date =
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, true, true, true, false,
      false)), EmptyString)))))))))))))))))))); ref_target = (String ((Ascii
      (true, false, true, true, false, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))); ref_charges = []; ref_result =
      (String ((Ascii (false, true, true, true, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val house_referral_giuliani : coq_CriminalReferral **)

  let house_referral_giuliani =
    { ref_source = (String ((Ascii (false, false, false, true, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); ref_date =
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, true, true, true, false,
      false)), EmptyString)))))))))))))))))))); ref_target = (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))); ref_charges = []; ref_result =
      (String ((Ascii (false, true, true, true, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val house_referrals : coq_CriminalReferral list **)

  let house_referrals =
    house_referral_trump::(house_referral_eastman::(house_referral_meadows::(house_referral_giuliani::[])))

  type coq_FederalCase = { fc_name : string; fc_docket : string;
                           fc_defendant : string;
                           fc_prosecutor : coq_ProsecutingEntity;
                           fc_charges : federalCharge list;
                           fc_indictment_date : string;
                           fc_status : coq_CaseStatus; fc_disposition : 
                           string }

  (** val usa_v_trump_dc : coq_FederalCase **)

  let usa_v_trump_dc =
    { fc_name = (String ((Ascii (true, false, true, false, true, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); fc_docket =
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, true, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (true, false, true, false, true, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, false, true,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, false, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false, true,
      false, false, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (true, false, false,
      true, false, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))); fc_defendant =
      (String ((Ascii (false, false, true, false, false, false, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))); fc_prosecutor =
      DOJ_SpecialCounsel; fc_charges =
      (Charge_18USC1512c2::(Charge_18USC371::(Charge_18USC372::[])));
      fc_indictment_date = (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, true, false, false,
      true, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, true, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); fc_status = CaseWithdrawn;
      fc_disposition = (String ((Ascii (true, true, true, false, true, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, false, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, true, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, true, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, false, true, false)),
      (String ((Ascii (false, true, false, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  type coq_StateCase = { sc_name : string; sc_docket : string;
                         sc_defendant : string;
                         sc_prosecutor : coq_ProsecutingEntity;
                         sc_charges_count : int; sc_defendants_count : 
                         int; sc_indictment_date : string;
                         sc_status : coq_CaseStatus; sc_disposition : 
                         string }

  (** val georgia_v_trump : coq_StateCase **)

  let georgia_v_trump =
    { sc_name = (String ((Ascii (true, true, false, false, true, false, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      true, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, false, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      sc_docket = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (true, true, false, false, true, true,
      false, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, false, false,
      true, false)), (String ((Ascii (true, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, true, true, true,
      false, false)), (String ((Ascii (false, false, false, true, true, true,
      false, false)), (String ((Ascii (true, false, false, true, true, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, false, true,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      false, false, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      sc_defendant = (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false, true,
      false, false, true, false)), (String ((Ascii (false, true, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, false, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      sc_prosecutor = FultonCountyDA; sc_charges_count = ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0))))))))))))))))))))))))))))))))))))))))); sc_defendants_count =
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0))))))))))))))))))); sc_indictment_date = (String ((Ascii (false,
      true, false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (true, true, false, false, true, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); sc_status = CaseDismissed;
      sc_disposition = (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, false, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false, true,
      true, false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (true, false, false, false, false, false, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, true, false, true, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, true,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  type coq_DisbarmentProceeding = { disbar_attorney : string;
                                    disbar_bar : coq_ProsecutingEntity;
                                    disbar_filing_date : string;
                                    disbar_status : coq_CaseStatus;
                                    disbar_outcome : string }

  (** val eastman_disbarment : coq_DisbarmentProceeding **)

  let eastman_disbarment =
    { disbar_attorney = (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, false, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), EmptyString))))))))))))))))))))))));
      disbar_bar = StateBarCalifornia; disbar_filing_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, true, true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); disbar_status = CaseConviction;
      disbar_outcome = (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, false, false)), (String ((Ascii (true,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii (true,
      true, true, false, true, true, false, false)), (String ((Ascii (true,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (true, true, false, true, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, true, false, false, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val giuliani_disbarment_ny : coq_DisbarmentProceeding **)

  let giuliani_disbarment_ny =
    { disbar_attorney = (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, false,
      false, false, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), EmptyString))))))))))))))))))))))))));
      disbar_bar = StateBarNewYork; disbar_filing_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, true, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); disbar_status = CaseConviction;
      disbar_outcome = (String ((Ascii (false, false, true, true, false,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (true, false, false, false, true, true, false, false)), (String ((Ascii
      (true, true, false, true, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (true, true, true, false, true, true,
      false, false)), (String ((Ascii (true, false, true, true, false, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val giuliani_disbarment_dc : coq_DisbarmentProceeding **)

  let giuliani_disbarment_dc =
    { disbar_attorney = (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, false,
      false, false, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), EmptyString))))))))))))))))))))))))));
      disbar_bar = DOJ_MainJustice; disbar_filing_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, true, true, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, true, false, false, true, true, false,
      false)), EmptyString)))))))))))))))))))); disbar_status =
      CaseConviction; disbar_outcome = (String ((Ascii (false, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, true, false, false, true, false)), (String ((Ascii
      (true, false, false, true, true, false, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (false, true, false, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val ellis_disbarment : coq_DisbarmentProceeding **)

  let ellis_disbarment =
    { disbar_attorney = (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, false, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), EmptyString))))))))))))))))))))));
      disbar_bar = StateBarColorado; disbar_filing_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (true, true, true, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, true, true, true, false, false)),
      EmptyString)))))))))))))))))))); disbar_status = CaseConviction;
      disbar_outcome = (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, true, true, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (true, true, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val disbarment_proceedings : coq_DisbarmentProceeding list **)

  let disbarment_proceedings =
    eastman_disbarment::(giuliani_disbarment_ny::(giuliani_disbarment_dc::(ellis_disbarment::[])))

  type coq_SpecialCounselReport = { scr_counsel : string;
                                    scr_release_date : string;
                                    scr_pages : int; scr_conclusion : 
                                    string; scr_source : string }

  (** val smith_final_report : coq_SpecialCounselReport **)

  let smith_final_report =
    { scr_counsel = (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), EmptyString))))))))))))))))))));
      scr_release_date = (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, true, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (true, true,
      true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); scr_pages = ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      scr_conclusion = (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, true, true,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, false, true, false, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      scr_source = (String ((Ascii (false, false, true, false, false, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, true, false, false,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      false, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, true, false, true, false, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, true, true,
      false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
 end

module CausalStructure =
 struct
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

  (** val speech_enables_march : coq_EventLink **)

  let speech_enables_march =
    { link_from = SpeechEnds; link_to = BarricadeBreach; link_relation =
      Enables; link_evidence = (String ((Ascii (false, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      true, true, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, false, false, false, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, true, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val barricade_enables_building_breach : coq_EventLink **)

  let barricade_enables_building_breach =
    { link_from = BarricadeBreach; link_to = BuildingBreach; link_relation =
      Enables; link_evidence = (String ((Ascii (false, true, false, false,
      true, false, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val building_breach_causes_recess : coq_EventLink **)

  let building_breach_causes_recess =
    { link_from = BuildingBreach; link_to = JointSessionRecesses;
      link_relation = Causes; link_evidence = (String ((Ascii (false, true,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val breach_causes_evacuation : coq_EventLink **)

  let breach_causes_evacuation =
    { link_from = BuildingBreach; link_to = Evacuation; link_relation =
      Causes; link_evidence = (String ((Ascii (false, true, true, false,
      true, false, true, false)), (String ((Ascii (false, false, false,
      false, true, false, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, false, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val tweet_motivates_violence : coq_EventLink **)

  let tweet_motivates_violence =
    { link_from = TweetPosted; link_to = OfficerAssault; link_relation =
      Motivates; link_evidence = (String ((Ascii (false, false, true, false,
      true, false, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false, true,
      false, true, true, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (true, true, false, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val guard_denial_facilitates_breach : coq_EventLink **)

  let guard_denial_facilitates_breach =
    { link_from = GuardDenied; link_to = PoliceLineCollapse; link_relation =
      Facilitates; link_evidence = (String ((Ascii (false, false, true, true,
      false, false, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, true, false, false, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, false, false,
      true, false)), (String ((Ascii (false, false, false, false, true,
      false, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val causal_links : coq_EventLink list **)

  let causal_links =
    speech_enables_march::(barricade_enables_building_breach::(building_breach_causes_recess::(breach_causes_evacuation::(tweet_motivates_violence::(guard_denial_facilitates_breach::[])))))

  type coq_CrowdEstimate = { ce_location : dCLocation; ce_time : minute;
                             ce_count_low : int; ce_count_high : int;
                             ce_source : string }

  (** val ellipse_rally_crowd : coq_CrowdEstimate **)

  let ellipse_rally_crowd =
    { ce_location = WhiteHouseEllipse; ce_time = t_1200; ce_count_low =
      (of_num_uint (UIntDecimal (D1 (D0 (D0 (D0 (D0 Nil)))))));
      ce_count_high =
      (of_num_uint (UIntDecimal (D1 (D5 (D0 (D0 (D0 Nil))))))); ce_source =
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, true, false, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, true, false, false, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val capitol_grounds_crowd : coq_CrowdEstimate **)

  let capitol_grounds_crowd =
    { ce_location = CapitolGrounds; ce_time = t_1400; ce_count_low =
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      ce_count_high = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      ce_source = (String ((Ascii (true, false, true, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, true, false,
      true, false)), (String ((Ascii (true, true, false, false, false, false,
      true, false)), (String ((Ascii (false, false, false, false, true,
      false, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val inside_building_crowd : coq_CrowdEstimate **)

  let inside_building_crowd =
    { ce_location = (CapitolBuilding Rotunda); ce_time = t_1430;
      ce_count_low = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      ce_count_high = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      ce_source = (String ((Ascii (false, true, true, false, true, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val crowd_estimates : coq_CrowdEstimate list **)

  let crowd_estimates =
    ellipse_rally_crowd::(capitol_grounds_crowd::(inside_building_crowd::[]))

  type coq_WeaponEvidence = { we_type : string; we_count : int;
                              we_location : dCLocation; we_seized : bool;
                              we_source : string }

  (** val flagpoles : coq_WeaponEvidence **)

  let flagpoles =
    { we_type = (String ((Ascii (false, true, true, false, false, false,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))));
      we_count = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      we_location = CapitolGrounds; we_seized = false; we_source = (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, false, true, false)),
      (String ((Ascii (false, true, false, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), EmptyString)))))))))))))))))))))))))))))))))))))))) }

  (** val bear_spray : coq_WeaponEvidence **)

  let bear_spray =
    { we_type = (String ((Ascii (false, true, false, false, false, false,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      we_count = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
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
      0)))))))))))))))))))))))))))))))))))))))))))))))))); we_location =
      (CapitolBuilding WestFront); we_seized = true; we_source = (String
      ((Ascii (true, false, true, false, true, false, true, false)), (String
      ((Ascii (true, true, false, false, true, false, true, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (false, false, false, false, true, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (true, true, true, false, false, true,
      true, false)), EmptyString)))))))))))))))))))))))))))))))))) }

  (** val tasers : coq_WeaponEvidence **)

  let tasers =
    { we_type = (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      false, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, true, true,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), EmptyString))))))))))))))))))))))))))))))));
      we_count = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) 0)))))))))))); we_location = CapitolGrounds;
      we_seized = true; we_source = (String ((Ascii (false, false, true,
      false, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, false, true, false)), (String ((Ascii (false, true, false,
      true, false, false, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))) }

  (** val baseball_bats : coq_WeaponEvidence **)

  let baseball_bats =
    { we_type = (String ((Ascii (false, true, false, false, false, false,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, true, true, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), EmptyString))))))))))))))))))))))))));
      we_count = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) 0)))))))); we_location = (CapitolBuilding WestFront);
      we_seized = true; we_source = (String ((Ascii (false, true, true,
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
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, false, true, false)), (String ((Ascii (true,
      true, true, true, false, false, true, false)), (String ((Ascii (false,
      true, false, true, false, false, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))) }

  (** val knives : coq_WeaponEvidence **)

  let knives =
    { we_type = (String ((Ascii (true, true, false, true, false, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, true, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), EmptyString)))))))))))); we_count = ((fun n -> n + 1)
      ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1)
      ((fun n -> n + 1) 0)))))); we_location = CapitolGrounds; we_seized =
      true; we_source = (String ((Ascii (true, false, true, false, true,
      false, true, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (false, false, false, false,
      true, false, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))) }

  (** val firearms_seized : coq_WeaponEvidence **)

  let firearms_seized =
    { we_type = (String ((Ascii (false, true, true, false, false, false,
      true, false)), (String ((Ascii (true, false, false, true, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, true, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, false, true,
      false, true, false, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      we_count = ((fun n -> n + 1) ((fun n -> n + 1) ((fun n -> n + 1) 0)));
      we_location = CapitolGrounds; we_seized = true; we_source = (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (true, true, true, true, false, false, true, false)),
      (String ((Ascii (false, true, false, true, false, false, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, true, true, false, true,
      true, false)), (String ((Ascii (false, true, false, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, true, true,
      true, false)), (String ((Ascii (true, true, false, true, true, true,
      false, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, true,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val weapons_evidence : coq_WeaponEvidence list **)

  let weapons_evidence =
    flagpoles::(bear_spray::(tasers::(baseball_bats::(knives::(firearms_seized::[])))))
 end

module Sources =
 struct
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

  (** val src_house_final_report : coq_Source **)

  let src_house_final_report =
    { src_type = OfficialReport; src_title = (String ((Ascii (false, true,
      true, false, false, false, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, false, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, true, false, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, true, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, true, false, true, true, false, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, false, false, false, false,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (true, false, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, false, false, true,
      true, false)), (String ((Ascii (true, true, false, true, false, true,
      true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (false, false, false, true, false,
      false, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))); src_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); src_url = (None (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, true, false, true, true, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, false, true, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, true, false, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, false, false, false, true, false)), (String ((Ascii
      (false, false, false, false, true, false, true, false)), (String
      ((Ascii (true, true, true, true, false, false, true, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, true, false, true, false, false, true, false)), (String
      ((Ascii (false, true, true, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, true, false, false, true, false, true, false)), (String
      ((Ascii (true, false, true, false, false, false, true, false)), (String
      ((Ascii (false, false, false, false, true, false, true, false)),
      (String ((Ascii (true, true, true, true, false, false, true, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, true, false, false, false, true, false)),
      (String ((Ascii (false, false, false, false, true, false, true,
      false)), (String ((Ascii (true, true, true, true, false, false, true,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, false, true,
      false)), (String ((Ascii (false, true, true, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, true, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, false, false, true,
      false)), (String ((Ascii (false, false, false, false, true, false,
      true, false)), (String ((Ascii (true, true, true, true, false, false,
      true, false)), (String ((Ascii (false, true, false, false, true, false,
      true, false)), (String ((Ascii (false, false, true, false, true, false,
      true, false)), (String ((Ascii (false, true, true, true, false, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, false, false,
      true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_rhodes_indictment : coq_Source **)

  let src_rhodes_indictment =
    { src_type = CourtFiling; src_title = (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, false, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (true, true, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, false,
      false, true, false, false, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (true, true,
      false, false, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, false, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (true, false,
      false, false, false, false, true, false)), (String ((Ascii (false,
      false, false, false, true, false, true, false)), (String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (false, false, true, false, false, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, true, true, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = (None (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, true, false, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      true, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, false, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      false, true, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, true, true, true, false, false)), (String ((Ascii (true,
      true, false, false, true, true, false, false)), (String ((Ascii (true,
      false, false, false, true, true, false, false)), (String ((Ascii (true,
      true, true, true, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_tarrio_indictment : coq_Source **)

  let src_tarrio_indictment =
    { src_type = CourtFiling; src_title = (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, false, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, false, true, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (false, false,
      true, false, true, false, true, false)), (String ((Ascii (false, true,
      false, true, false, false, true, false)), (String ((Ascii (true, true,
      false, true, false, false, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (false, false, true, false, false, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, true, true, false,
      true, true, false, false)), EmptyString)))))))))))))))))))); src_url =
      (None (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, true, false, true, true, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, false, true, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (false, true, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, false, true, true, false,
      false)), (String ((Ascii (true, false, true, false, true, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (true, true, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, true, true, true, false,
      false)), (String ((Ascii (false, true, true, false, true, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_trump_indictment : coq_Source **)

  let src_trump_indictment =
    { src_type = CourtFiling; src_title = (String ((Ascii (true, false, true,
      false, true, false, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, false, true, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, true, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (false, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (true, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, true, false,
      false, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      false, true, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, false, true, false)), (String ((Ascii (true, true, false,
      false, true, false, true, false)), (String ((Ascii (true, true, false,
      false, false, false, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, true, false, true, false, false)), (String ((Ascii
      (false, false, true, false, false, false, true, false)), (String
      ((Ascii (false, true, true, true, false, true, false, false)), (String
      ((Ascii (false, false, true, false, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (true, false, false, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (false, false, true, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, false, true,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = (None (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, true, false, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      false, true, false)), (String ((Ascii (true, true, false, false, true,
      false, true, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (false, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (false, false, true, false, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, true, false, false, true,
      true, false, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, false, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, true,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (false, false, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_doj_ig_guard : coq_Source **)

  let src_doj_ig_guard =
    { src_type = OfficialReport; src_title = (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, true, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, false, false, false, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, false, true, false, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, true,
      false)), (String ((Ascii (false, true, true, false, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      true, false)), (String ((Ascii (false, false, false, true, false, true,
      true, false)), (String ((Ascii (true, false, true, false, false, true,
      true, false)), (String ((Ascii (false, false, false, false, false,
      true, false, false)), (String ((Ascii (false, false, true, false,
      false, false, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      false, false, true, false)), (String ((Ascii (true, true, true, false,
      false, true, false, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (false,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, true, false, true, false, false, true, false)), (String
      ((Ascii (true, false, false, false, false, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, true, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, false, false, false, true,
      false)), (String ((Ascii (false, true, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, true, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (true, true, false, false, true, true, true,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      false, true, false)), (String ((Ascii (false, false, true, false,
      false, false, true, false)), (String ((Ascii (true, false, false, true,
      false, false, true, false)), (String ((Ascii (true, true, true, false,
      false, false, true, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, false, false)), (String ((Ascii (false, false,
      false, false, true, true, false, false)), (String ((Ascii (false, true,
      false, false, true, true, false, false)), (String ((Ascii (true, true,
      false, false, true, true, false, false)), (String ((Ascii (true, false,
      true, true, false, true, false, false)), (String ((Ascii (true, false,
      false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii (true,
      false, true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))); src_date = (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, true, false, true, true, false, false)),
      EmptyString)))))))))))))))))))); src_url = (None (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, true, false, true, true, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, true, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, true, false, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (true, true, true, true, false, true, false, false)), (String ((Ascii
      (true, false, false, false, false, false, true, false)), (String
      ((Ascii (false, true, false, false, true, true, true, false)), (String
      ((Ascii (false, false, true, false, true, true, true, false)), (String
      ((Ascii (true, false, false, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (true, true, true, true, false, true, false, false)), (String
      ((Ascii (true, true, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, true, true, false, true, true, false, false)), (String
      ((Ascii (false, false, false, true, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (false, false, true, false, true, true, false, false)), (String
      ((Ascii (true, true, true, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_dc_ocme_babbitt : coq_Source **)

  let src_dc_ocme_babbitt =
    { src_type = OfficialReport; src_title = (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (true, true,
      false, false, false, false, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, false, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, true, false, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, false, false,
      false, true, false)), (String ((Ascii (false, false, false, true, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, false, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      true, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (true,
      false, true, false, false, false, true, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      false, false)), (String ((Ascii (true, false, false, false, true, true,
      false, false)), EmptyString))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, true, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = Some }

  (** val src_dc_ocme_sicknick : coq_Source **)

  let src_dc_ocme_sicknick =
    { src_type = OfficialReport; src_title = (String ((Ascii (false, false,
      true, false, false, false, true, false)), (String ((Ascii (true, true,
      false, false, false, false, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, true, true, true, false, false, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (false, true, true, false, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, false, true, true, false, false, true,
      false)), (String ((Ascii (true, false, true, false, false, true, true,
      false)), (String ((Ascii (false, false, true, false, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (true, true, false, false, false, true, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, true, false, false,
      false, true, false)), (String ((Ascii (false, false, false, true, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (false, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, false, false, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, true, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true,
      false, false, true, false, true, false)), (String ((Ascii (true, false,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (true, true, false, false, false,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (false, true, true,
      true, false, false, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, true, false, false, true, false)), (String ((Ascii (true,
      false, true, false, false, false, true, false)), (String ((Ascii
      (false, true, false, false, true, true, false, false)), (String ((Ascii
      (false, false, false, false, true, true, false, false)), (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (true, false, false, false, true, true, false, false)), (String
      ((Ascii (true, false, true, true, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, false, true, false, true, true,
      false, false)), (String ((Ascii (true, false, true, false, true, true,
      false, false)), EmptyString))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (false, false, true, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (true, false, false,
      false, true, true, false, false)), (String ((Ascii (true, false, false,
      true, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = Some }

  (** val src_cspan_rally : coq_Source **)

  let src_cspan_rally =
    { src_type = VideoEvidence; src_title = (String ((Ascii (true, true,
      false, false, false, false, true, false)), (String ((Ascii (true,
      false, true, true, false, true, false, false)), (String ((Ascii (true,
      true, false, false, true, false, true, false)), (String ((Ascii (false,
      false, false, false, true, false, true, false)), (String ((Ascii (true,
      false, false, false, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, false, true, false)), (String ((Ascii
      (false, false, true, true, false, true, false, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, false, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (false, false, true, false, true, false, true,
      false)), (String ((Ascii (false, true, false, false, true, true, true,
      false)), (String ((Ascii (true, false, true, false, true, true, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, true, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, false, false, true,
      false, true, false)), (String ((Ascii (true, false, false, false,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      true, true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      false, true, false, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, true, false, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)), (String ((Ascii
      (true, true, false, false, false, false, true, false)), (String ((Ascii
      (false, true, true, true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (false, true, true, false, true,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, true, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, false, false, true,
      false, false, true, false)), (String ((Ascii (false, false, true,
      false, false, false, true, false)), (String ((Ascii (false, true,
      false, true, true, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true,
      false, true, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, true, true, false, false)), (String ((Ascii (true,
      true, true, false, true, true, false, false)), (String ((Ascii (true,
      true, true, false, true, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, false, false)), (String ((Ascii (false,
      false, true, false, true, true, false, false)), (String ((Ascii (true,
      false, true, true, false, true, false, false)), (String ((Ascii (true,
      false, false, false, true, true, false, false)),
      EmptyString)))))))))))))))))))))))))))))))))))); src_date = (String
      ((Ascii (false, true, false, false, true, true, false, false)), (String
      ((Ascii (false, false, false, false, true, true, false, false)),
      (String ((Ascii (false, true, false, false, true, true, false, false)),
      (String ((Ascii (true, false, false, false, true, true, false, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, true, true,
      false, false)), (String ((Ascii (false, true, true, false, true, true,
      false, false)), EmptyString)))))))))))))))))))); src_url = (None
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, true, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, true, false, true, true, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (true, true, true, false, true, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, false, true, true, false, true, false, false)),
      (String ((Ascii (true, true, false, false, true, true, true, false)),
      (String ((Ascii (false, false, false, false, true, true, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, false, false, true, true, true, false)),
      (String ((Ascii (true, true, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (false, true, true, false, true, true, true, false)),
      (String ((Ascii (true, false, false, true, false, true, true, false)),
      (String ((Ascii (false, false, true, false, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, false, false)),
      (String ((Ascii (true, true, true, true, true, true, false, false)),
      (String ((Ascii (true, false, true, false, true, true, false, false)),
      (String ((Ascii (false, false, false, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)), (String ((Ascii (true, true, true, false, true, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, false,
      false)), (String ((Ascii (false, false, true, false, true, true, false,
      false)), (String ((Ascii (true, false, true, true, false, true, false,
      false)), (String ((Ascii (true, false, false, false, true, true, false,
      false)), (String ((Ascii (true, true, true, true, false, true, false,
      false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_wapo_georgia_audio : coq_Source **)

  let src_wapo_georgia_audio =
    { src_type = AudioRecording; src_title = (String ((Ascii (true, true,
      true, false, true, false, true, false)), (String ((Ascii (true, false,
      false, false, false, true, true, false)), (String ((Ascii (true, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      false, true, false, true, true, false)), (String ((Ascii (true, false,
      false, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (true, true,
      true, false, false, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      true, true, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      false, false, false, true, false, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (true,
      true, false, false, true, true, true, false)), (String ((Ascii (false,
      false, true, false, true, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (false, false, true, false, true, false, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, true, false, true, false, false)), (String ((Ascii
      (false, true, false, false, true, false, true, false)), (String ((Ascii
      (true, false, false, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (true, true, true, false, false, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (false, true, false, false, true, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (false, false, false, false, true, false, true, false)),
      (String ((Ascii (false, false, false, true, false, true, true, false)),
      (String ((Ascii (true, true, true, true, false, true, true, false)),
      (String ((Ascii (false, true, true, true, false, true, true, false)),
      (String ((Ascii (true, false, true, false, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, false, false, true,
      false)), (String ((Ascii (true, false, false, false, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, true,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (true, false, false, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (false, true, true, false, false,
      false, true, false)), (String ((Ascii (true, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, true, true, false, false)), (String ((Ascii (false,
      true, false, false, true, true, false, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, true, true, false, true, true, false)), (String ((Ascii
      (true, false, false, true, false, true, true, false)), (String ((Ascii
      (false, true, true, true, false, true, true, false)), (String ((Ascii
      (true, false, true, false, true, true, true, false)), (String ((Ascii
      (false, false, true, false, true, true, true, false)), (String ((Ascii
      (true, false, true, false, false, true, true, false)), (String ((Ascii
      (true, true, false, false, true, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (true, true, false,
      false, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = (None (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, true, false, true, true,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, true, false, true,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, false, true, false,
      true, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, false, false,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false, true,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, true, false, false, true,
      true, true, false)), (String ((Ascii (false, false, true, false, true,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, false, false)), (String ((Ascii (true, true, false, false, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, true, true, false,
      true, true, false)), (String ((Ascii (true, true, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, true, false)), (String ((Ascii (false, false, true, true,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (false, false, true, false,
      true, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      true, true, true, false)), (String ((Ascii (true, true, true, true,
      false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      true, false, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (false, true, true,
      true, false, true, true, false)), (String ((Ascii (true, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, false,
      false, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (false, false, false,
      false, true, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, false,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      false, false, true, true, false)), (String ((Ascii (true, false, false,
      true, false, true, true, false)), (String ((Ascii (true, false, false,
      false, false, true, true, false)), (String ((Ascii (true, false, true,
      true, false, true, false, false)), (String ((Ascii (false, true, true,
      false, true, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, false, true,
      false, true, true, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (true, true, true,
      true, false, true, false, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

  (** val src_smith_final_report : coq_Source **)

  let src_smith_final_report =
    { src_type = OfficialReport; src_title = (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)), (String
      ((Ascii (false, false, false, false, false, true, false, false)),
      (String ((Ascii (false, true, false, true, false, false, true, false)),
      (String ((Ascii (true, false, false, false, false, true, true, false)),
      (String ((Ascii (true, true, false, false, false, true, true, false)),
      (String ((Ascii (true, true, false, true, false, true, true, false)),
      (String ((Ascii (false, false, false, false, false, true, false,
      false)), (String ((Ascii (true, true, false, false, true, false, true,
      false)), (String ((Ascii (true, false, true, true, false, true, true,
      false)), (String ((Ascii (true, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, false, true, true, true,
      false)), (String ((Ascii (false, false, false, true, false, true, true,
      false)), (String ((Ascii (false, false, true, true, false, true, false,
      false)), (String ((Ascii (false, false, false, false, false, true,
      false, false)), (String ((Ascii (false, true, true, false, false,
      false, true, false)), (String ((Ascii (true, false, false, true, false,
      true, true, false)), (String ((Ascii (false, true, true, true, false,
      true, true, false)), (String ((Ascii (true, false, false, false, false,
      true, true, false)), (String ((Ascii (false, false, true, true, false,
      true, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (false, true, false,
      false, true, false, true, false)), (String ((Ascii (true, false, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, true, true, true, false)), (String ((Ascii (true, true,
      true, true, false, true, true, false)), (String ((Ascii (false, true,
      false, false, true, true, true, false)), (String ((Ascii (false, false,
      true, false, true, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (false,
      true, true, false, true, false, true, false)), (String ((Ascii (true,
      true, true, true, false, true, true, false)), (String ((Ascii (false,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, true, true, true, false)), (String ((Ascii (true,
      false, true, true, false, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (false,
      false, false, false, false, true, false, false)), (String ((Ascii
      (true, false, false, true, false, false, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_identifier = (String ((Ascii (false, false, true, false, false,
      false, true, false)), (String ((Ascii (true, true, true, true, false,
      false, true, false)), (String ((Ascii (false, true, false, true, false,
      false, true, false)), (String ((Ascii (false, false, false, false,
      false, true, false, false)), (String ((Ascii (true, true, true, true,
      false, false, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (false, true, true, false,
      false, true, true, false)), (String ((Ascii (true, false, false, true,
      false, true, true, false)), (String ((Ascii (true, true, false, false,
      false, true, true, false)), (String ((Ascii (true, false, true, false,
      false, true, true, false)), (String ((Ascii (false, false, false,
      false, false, true, false, false)), (String ((Ascii (true, true, true,
      true, false, true, true, false)), (String ((Ascii (false, true, true,
      false, false, true, true, false)), (String ((Ascii (false, false,
      false, false, false, true, false, false)), (String ((Ascii (true, true,
      false, false, true, false, true, false)), (String ((Ascii (false,
      false, false, false, true, true, true, false)), (String ((Ascii (true,
      false, true, false, false, true, true, false)), (String ((Ascii (true,
      true, false, false, false, true, true, false)), (String ((Ascii (true,
      false, false, true, false, true, true, false)), (String ((Ascii (true,
      false, false, false, false, true, true, false)), (String ((Ascii
      (false, false, true, true, false, true, true, false)), (String ((Ascii
      (false, false, false, false, false, true, false, false)), (String
      ((Ascii (true, true, false, false, false, false, true, false)), (String
      ((Ascii (true, true, true, true, false, true, true, false)), (String
      ((Ascii (true, false, true, false, true, true, true, false)), (String
      ((Ascii (false, true, true, true, false, true, true, false)), (String
      ((Ascii (true, true, false, false, true, true, true, false)), (String
      ((Ascii (true, false, true, false, false, true, true, false)), (String
      ((Ascii (false, false, true, true, false, true, true, false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
      src_date = (String ((Ascii (false, true, false, false, true, true,
      false, false)), (String ((Ascii (false, false, false, false, true,
      true, false, false)), (String ((Ascii (false, true, false, false, true,
      true, false, false)), (String ((Ascii (true, false, true, false, true,
      true, false, false)), (String ((Ascii (true, false, true, true, false,
      true, false, false)), (String ((Ascii (false, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, false, false,
      true, true, false, false)), (String ((Ascii (true, false, true, true,
      false, true, false, false)), (String ((Ascii (false, false, false,
      false, true, true, false, false)), (String ((Ascii (true, true, true,
      false, true, true, false, false)), EmptyString))))))))))))))))))));
      src_url = Some }

  (** val primary_sources : coq_Source list **)

  let primary_sources =
    src_house_final_report::(src_rhodes_indictment::(src_tarrio_indictment::(src_trump_indictment::(src_doj_ig_guard::(src_dc_ocme_babbitt::(src_dc_ocme_sicknick::(src_cspan_rally::(src_wapo_georgia_audio::(src_smith_final_report::[])))))))))
 end

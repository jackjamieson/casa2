module Items

open Casanova.Core
open Casanova.StandardLibrary.Core
open Casanova.Math
open Casanova.Coroutines
open Casanova.StandardLibrary.Physics
open MathNet.Numerics.LinearAlgebra.Single
open Proxies
open Casanova.Utilities


///internal use only
type [<CasanovaEntity>] Object< ^source, ^target, ^world> =
  {
    SameTypeProxies           : RuleTable<Proxy< ^source, ^source, ^world>>
    DifferentTypeProxies1     : RuleTable<Proxy< ^source, ^target, ^world>>
    DifferentTypeProxies2     : RuleTable<Ref<Proxy< ^target, ^source, ^world>>>

    SameActionProxies         : RuleTable<ActionTresholdProxy< ^source, ^source, ^world>>
    DifferentActionProxies1   : RuleTable<ActionTresholdProxy< ^source, ^target, ^world>>
    DifferentActionProxies2   : RuleTable<Ref<ActionTresholdProxy< ^target, ^source, ^world>>>

    SameResourceProxies       : RuleTable<ResourcesTresholdProxy< ^source, ^source, ^world>>
    DifferentResourceProxies1 : RuleTable<ResourcesTresholdProxy< ^source, ^target, ^world>>
    DifferentResourceProxies2 : RuleTable<Ref<ResourcesTresholdProxy< ^target, ^source, ^world>>>
  } with
  member inline this.Count =
    (this.SameTypeProxies.Value |> Seq.length) +
    (this.DifferentTypeProxies1.Value |> Seq.length) +

    (this.SameActionProxies.Value |> Seq.length) +
    (this.DifferentActionProxies1.Value |> Seq.length) +

    (this.SameResourceProxies.Value |> Seq.length) +
    (this.DifferentResourceProxies1.Value |> Seq.length)

  static member inline SameTypeProxiesRule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for m_p in self.SameTypeProxies do
        if not m_p.IsDead then yield m_p
    }

  static member inline DifferentTypeProxies1Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_m_p in self.DifferentTypeProxies1 do
        if not i_m_p.IsDead then yield i_m_p
    }
  static member inline DifferentTypeProxies2Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_m_p in self.DifferentTypeProxies2 do
        if not i_m_p.Value.IsDead then yield i_m_p
    }

  static member inline SameActionProxiesRule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for m_a_p in self.SameActionProxies do
        if not m_a_p.IsDead then yield m_a_p
    }

  static member inline DifferentActionProxies1Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_a_p in self.DifferentActionProxies1 do
        if not i_a_p.IsDead then yield i_a_p
    }

  static member inline DifferentActionProxies2Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_a_p in self.DifferentActionProxies2 do
        if not i_a_p.Value.IsDead then yield i_a_p
    }

  static member inline SameResourceProxiesRule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for m_r_p in self.SameResourceProxies do
        if not m_r_p.IsDead then yield m_r_p
    }

  static member inline DifferentResourceProxies1Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_r_p in self.DifferentResourceProxies1 do
        if not i_r_p.IsDead then yield i_r_p
    }

  static member inline DifferentResourceProxies2Rule(world : ^world, self : Object< ^source, ^target, ^world>, dt : float32<s>) =
    seq{
      for i_r_p in self.DifferentResourceProxies2 do
        if not i_r_p.Value.IsDead then yield i_r_p
    }

type [<CasanovaEntity>] Resource< ^world> =
  {
    DefenceResourcesStartIndex  : int
    AttackResourcesStartIndex   : int
    AttackResourcesNumber       : int
    Resources           : Rule<SparseVector>
    TakenResources      : Rule<SparseVector>
    GivenResources      : Rule<SparseVector>
  }  with
  static member inline ResourcesRule(world : ^world, self : Resource< ^world>, dt : float32<s>) =           
      let res = !self.TakenResources + !self.GivenResources + !self.Resources

      for i = 0 to res.Count - 1 do
        if res.[i] > 0.0f then
          res.[i] <- min 100.0f res.[i]
        else res.[i] <- 0.0f
      SparseVector(res)

let inline internal NewResource def_res_start att_res_start att_res_num res =
    {
      DefenceResourcesStartIndex  = def_res_start
      AttackResourcesStartIndex   = att_res_start
      AttackResourcesNumber       = att_res_num
      Resources                   = Rule.Create(fun () -> res)
      TakenResources              = Rule.Create(fun () -> SparseVector(res.Count))
      GivenResources              = Rule.Create(fun () -> SparseVector(res.Count))
    } 

let inline internal NewObject< ^source, ^target, ^world> () : Object< ^source, ^target, ^world> =
    {
      SameTypeProxies           = RuleTable.Create(fun () -> Seq.empty)
      DifferentTypeProxies1      = RuleTable.Create(fun () -> Seq.empty)
      DifferentTypeProxies2      = RuleTable.Create(fun () -> Seq.empty)

      SameActionProxies         = RuleTable.Create(fun () -> Seq.empty)
      DifferentActionProxies1    = RuleTable.Create(fun () -> Seq.empty)
      DifferentActionProxies2    = RuleTable.Create(fun () -> Seq.empty)

      SameResourceProxies       = RuleTable.Create(fun () -> Seq.empty)
      DifferentResourceProxies1  = RuleTable.Create(fun () -> Seq.empty)
      DifferentResourceProxies2  = RuleTable.Create(fun () -> Seq.empty)
    } 

type [<CasanovaEntity; ReferenceEquality >] MovableObject< ^world> =
  {
    Entity          : PhysicalEntity
    Proxies         : Object< MovableObject< ^world>, ImmovableObject< ^world>, ^world>

    SameTargetTypeProxies         : List<float32 * string * ( MovableObject< ^world> -> MovableObject< ^world> -> Proxy< MovableObject< ^world>, MovableObject< ^world>, ^world>)>
    DiffTargetTypeProxies         : List<float32 * string * ( ImmovableObject< ^world> -> MovableObject< ^world> -> Proxy< ImmovableObject< ^world>, MovableObject< ^world>, ^world>)>

    SameTargetTypeActionProxies   : List<float32 * string * ( MovableObject< ^world> -> Proxy< MovableObject< ^world>, MovableObject< ^world>, ^world>)>
    DiffTargetTypeActionProxies   : List<float32 * string * ( ImmovableObject< ^world> -> MovableObject< ^world> -> Proxy< ImmovableObject< ^world>, MovableObject< ^world>, ^world>)>
    
    SameTargetTypeResourceProxies : List<float32 * string * ( MovableObject< ^world> -> Proxy< MovableObject< ^world>, MovableObject< ^world>, ^world>)>
    DiffTargetTypeResourceProxies : List<float32 * string * ( ImmovableObject< ^world> -> MovableObject< ^world> -> Proxy< ImmovableObject< ^world>, MovableObject< ^world>, ^world>)>

    EntityResourceNumber : int
    EntityResourceNames  : string list
    EntityResource       : Resource< ^world>
  } with


  static member inline EntityResourceGivenResourcesRule(world : ^world, self : MovableObject< ^world>, dt : float32<s>) =
  
    let taken_res =
        seq{
          for m_to_imm_r in self.Proxies.DifferentResourceProxies1 do
            yield !m_to_imm_r.SourceOutput       
        }
    let taken_res_sum = taken_res |> Seq.fold(fun s t -> t + s) (SparseVector(self.EntityResourceNumber))    
    taken_res_sum

  member inline this.ClearProxies() =
    this.Proxies.DifferentActionProxies1.Clear()
    this.Proxies.DifferentResourceProxies1.Clear()
    this.Proxies.DifferentTypeProxies1.Clear()

    for p in this.Proxies.DifferentActionProxies2 do
      p.Value.ActionStatus := Status.Died

    for p in this.Proxies.DifferentResourceProxies2 do
      p.Value.ActionStatus := Status.Died

    for p in this.Proxies.DifferentTypeProxies2 do
      p.Value.ActionStatus := Status.Died

    this.Proxies.SameActionProxies.Clear()
    this.Proxies.SameResourceProxies.Clear()
    this.Proxies.SameTypeProxies.Clear()

  member inline this.IsAlive =
    this.EntityResource.Resources.Value.[this.EntityResource.Resources.Value.Count - 1] > 0.0f

  static member inline Create< ^world> resources_names def_res_start att_res_start att_res_num res same_proxies diff_proxies same_actions diff_actions same_res diff_res position max_speed =
    {
      Entity            = PhysicalEntity.Create(position, max_speed)

      Proxies           = NewObject<MovableObject< ^world>, ImmovableObject< ^world>, ^world>()   
       
      SameTargetTypeProxies         = same_proxies
      DiffTargetTypeProxies         = diff_proxies

      SameTargetTypeActionProxies   = same_actions
      DiffTargetTypeActionProxies   = diff_actions
    
      SameTargetTypeResourceProxies = same_res
      DiffTargetTypeResourceProxies = diff_res
      
      EntityResourceNumber  = resources_names |> Seq.length
      EntityResourceNames   = resources_names
      EntityResource        = NewResource def_res_start att_res_start att_res_num res
    }  
and ContainerProxy = Movable | Immovable

and [<CasanovaEntity; ReferenceEquality >] ImmovableObject< ^world> =
  {
    Position          : Vector2<m>
    Proxies           : Object< ImmovableObject< ^world>, MovableObject< ^world>, ^world>
    


    SameTargetTypeProxies         : List<float32 * ContainerProxy * string * ( ImmovableObject< ^world> -> ImmovableObject< ^world> -> Proxy< ImmovableObject< ^world>, ImmovableObject< ^world>, ^world>)>
    DiffTargetTypeProxiesImmovToMov         : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> Proxy< ImmovableObject< ^world>, MovableObject< ^world>, ^world>)>
    DiffTargetTypeProxiesMovToImmov         : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> Proxy< MovableObject< ^world>, ImmovableObject< ^world>, ^world>)>

    SameTargetTypeActionProxies   : List<float32 * ContainerProxy * string * ( ImmovableObject< ^world> -> ImmovableObject< ^world> -> ActionTresholdProxy< ImmovableObject< ^world>, ImmovableObject< ^world>, ^world>)>
    DiffTargetTypeActionProxiesImmovToMov   : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> ActionTresholdProxy< ImmovableObject< ^world>, MovableObject< ^world>,^world>)>
    DiffTargetTypeActionProxiesMovToImmov   : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> ActionTresholdProxy< MovableObject< ^world>, ImmovableObject< ^world>, ^world>)>
    
    SameTargetTypeResourceProxies : List<float32 * ContainerProxy * string  * ( ImmovableObject< ^world> -> ImmovableObject< ^world> -> ResourcesTresholdProxy< ImmovableObject< ^world>, ImmovableObject< ^world>, ^world>)>
    DiffTargetTypeResourceProxiesImmovToMov : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> ResourcesTresholdProxy< ImmovableObject< ^world>, MovableObject< ^world>, ^world>)>
    DiffTargetTypeResourceProxiesMovToImmov : List<float32 * ContainerProxy * string * ( MovableObject< ^world> -> ImmovableObject< ^world> -> ResourcesTresholdProxy< MovableObject< ^world>, ImmovableObject< ^world>, ^world>)>


    EntityResourceNumber : int
    EntityResourceNames  : string list
    EntityResource       : Resource< ^world>
  } with


  member inline this.IsAlive =
    this.EntityResource.Resources.Value.[this.EntityResource.Resources.Value.Count - 1] > 0.0f

  static member inline Create< ^world> resources_names def_res_start att_res_start att_res_num res same_proxies diff_proxies1 diff_proxies2 same_actions diff_actions1 diff_actions2 same_res diff_res1 diff_res2 position =
    {
      Position          = position

      Proxies           = NewObject< ImmovableObject< ^world>, MovableObject< ^world>, ^world>()    
      
      SameTargetTypeProxies         = same_proxies
      DiffTargetTypeProxiesImmovToMov = diff_proxies1
      DiffTargetTypeProxiesMovToImmov = diff_proxies2

      SameTargetTypeActionProxies   = same_actions
      DiffTargetTypeActionProxiesImmovToMov   = diff_actions1
      DiffTargetTypeActionProxiesMovToImmov   = diff_actions2
    
      SameTargetTypeResourceProxies = same_res
      DiffTargetTypeResourceProxiesImmovToMov = diff_res1
      DiffTargetTypeResourceProxiesMovToImmov = diff_res2

      EntityResourceNumber  = resources_names |> Seq.length
      EntityResourceNames   = resources_names
      EntityResource        = NewResource def_res_start att_res_start att_res_num res
    }  

  static member inline EntityResourceGivenResourcesRule(world : ^world, self : ImmovableObject< ^world>, dt : float32<s>) =
    
    let taken_res =
        seq{          
          for imm_to_m_p in !self.Proxies.DifferentTypeProxies1 do              
            yield !imm_to_m_p.SourceOutput

          for imm_to_m_r in self.Proxies.DifferentResourceProxies1 do              
            yield !imm_to_m_r.SourceOutput
        }

    let taken_res_sum = taken_res |> Seq.fold(fun s t -> t + s) (SparseVector(self.EntityResourceNumber))
    
    taken_res_sum

  member inline this.Select i  
                            (maybe_source_movable : MovableObject< ^world> option)
                            (maybe_source_immovable : ImmovableObject< ^world> option) =
      begin
        let i = var i

        for _,_,_,a in this.SameTargetTypeProxies do          
          if !i = 0 && maybe_source_immovable.IsSome then 
            maybe_source_immovable.Value.Proxies.SameTypeProxies.Add(a this maybe_source_immovable.Value)
          i := !i - 1
                  
        for _,container,_,a in this.DiffTargetTypeProxiesImmovToMov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 -> 
               let proxie = a maybe_source_movable.Value this
               maybe_source_immovable.Value.Proxies.DifferentTypeProxies1.Add(proxie)
               source_movable.Proxies.DifferentTypeProxies2.Add(ref proxie)
            | _ -> ()
          else 
            if !i = 0 then
              this.Proxies.DifferentTypeProxies1.Add(a maybe_source_movable.Value this)                      
          i := !i - 1

        for _,container,_,a in this.DiffTargetTypeProxiesMovToImmov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 ->
               source_movable.Proxies.DifferentTypeProxies1.Add(a maybe_source_movable.Value this)          
            | _ -> ()
          else 
            if !i = 0 then
              let proxie = a maybe_source_movable.Value this
              maybe_source_movable.Value.Proxies.DifferentTypeProxies1.Add(proxie)
              this.Proxies.DifferentTypeProxies2.Add(ref proxie)
          i := !i - 1
          
        for _,_,_,a in this.SameTargetTypeActionProxies do
          if !i = 0 && maybe_source_immovable.IsSome then 
            maybe_source_immovable.Value.Proxies.SameActionProxies.Add(a maybe_source_immovable.Value this)
          i := !i - 1
          
        for _,container,_,a in this.DiffTargetTypeActionProxiesImmovToMov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 ->
               let proxie = a maybe_source_movable.Value this
               maybe_source_immovable.Value.Proxies.DifferentActionProxies1.Add(proxie)
               source_movable.Proxies.DifferentActionProxies2.Add(ref proxie)
            | _ -> ()
          else 
            if !i = 0 then
              this.Proxies.DifferentActionProxies1.Add(a maybe_source_movable.Value this)                      
          i := !i - 1

        for _,container,_,a in this.DiffTargetTypeActionProxiesMovToImmov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 ->
               source_movable.Proxies.DifferentActionProxies1.Add(a maybe_source_movable.Value this)          
            | _ -> ()
          else 
            if !i = 0 then
              let proxie = a maybe_source_movable.Value this
              maybe_source_movable.Value.Proxies.DifferentActionProxies1.Add(proxie)
              this.Proxies.DifferentActionProxies2.Add(ref proxie)
          i := !i - 1


        for _,_,_,a in this.SameTargetTypeResourceProxies do
          if !i = 0 && maybe_source_immovable.IsSome then  
            maybe_source_immovable.Value.Proxies.SameResourceProxies.Add(a maybe_source_immovable.Value this)
          i := !i - 1


        for _,container,_,a in this.DiffTargetTypeResourceProxiesImmovToMov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 ->
                let proxie = a maybe_source_movable.Value this
                maybe_source_immovable.Value.Proxies.DifferentResourceProxies1.Add(proxie)
                source_movable.Proxies.DifferentResourceProxies2.Add(ref proxie)
            | _ -> ()
          else 
            if !i = 0 then 
               this.Proxies.DifferentResourceProxies1.Add(a maybe_source_movable.Value this)
          i := !i - 1

        for _,container,_,a in this.DiffTargetTypeResourceProxiesMovToImmov do
          if container = ContainerProxy.Movable then
            match maybe_source_movable, !i with
            | Some source_movable, 0 ->
               source_movable.Proxies.DifferentResourceProxies1.Add(a maybe_source_movable.Value this)          
            | _ -> ()
          else 
            if !i = 0 then
              let proxie = a maybe_source_movable.Value this
              maybe_source_movable.Value.Proxies.DifferentResourceProxies1.Add(proxie)
              this.Proxies.DifferentResourceProxies2.Add(ref proxie)

          i := !i - 1


      end

    member inline this.GetAvailableActionsNames =
      seq{ 
        for s,_,d,_ in this.SameTargetTypeProxies do yield s,d
        for s,_,d,_ in this.DiffTargetTypeProxiesImmovToMov do yield s,d
        for s,_,d,_ in this.DiffTargetTypeProxiesMovToImmov do yield s,d
        for s,_,d,_ in this.SameTargetTypeActionProxies do yield s,d
        for s,_,d,_ in this.DiffTargetTypeActionProxiesImmovToMov do yield s,d
        for s,_,d,_ in this.DiffTargetTypeActionProxiesMovToImmov do yield s,d
        for s,_,d,_ in this.SameTargetTypeResourceProxies do yield s,d
        for s,_,d,_ in this.DiffTargetTypeResourceProxiesImmovToMov do yield s,d
        for s,_,d,_ in this.DiffTargetTypeResourceProxiesMovToImmov do yield s,d }

    member inline this.CountAvailableActions =
      this.SameTargetTypeProxies.Length +
      this.DiffTargetTypeProxiesImmovToMov.Length +
      this.DiffTargetTypeProxiesMovToImmov.Length +
      this.SameTargetTypeActionProxies.Length +
      this.DiffTargetTypeActionProxiesImmovToMov.Length +
      this.DiffTargetTypeActionProxiesMovToImmov.Length +
      this.SameTargetTypeResourceProxies.Length +
      this.DiffTargetTypeResourceProxiesMovToImmov.Length +
      this.DiffTargetTypeResourceProxiesImmovToMov.Length
  

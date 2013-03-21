module Proxies

open Casanova.Core
open Casanova.StandardLibrary.Core
open Casanova.Math
open Casanova.Coroutines
open Casanova.StandardLibrary.Physics
open MathNet.Numerics.LinearAlgebra.Single
open Casanova.Utilities


let round_res (resources : SparseVector) =
  for i = 0 to resources.Count - 1 do
    if resources.[i] < 10.0f then resources.[i] <- 0.0f
  resources

let greater_or_equal (u : SparseVector) (v : SparseVector) =
  let mutable res = true
  for i = 0 to u.Count - 1 do
    if u.[i] < v.[i] then res <- false
  res

type EnableStatus = { Enabled : Var<bool> }

and Status =
  | Died
  | Automatic
  | Manual of EnableStatus

type ProxyOperations< ^source, ^target, ^world> =
  {

    IsOwnerDead         : Ref< ^source> -> bool

    GetSourcePos        : Ref< ^source> -> Vector2<m>
    GetTargetPos        : Ref< ^target> -> Vector2<m>

    CountEnabledProxies : Ref< ^source> -> Ref< ^world> -> int

    GetLocalResource    : Ref< ^source> -> SparseVector
    GetTargetResource   : Ref< ^target> -> SparseVector

    GetEntities         : Ref< ^source> -> float32<m> -> Ref< ^world> -> seq<Ref< ^target>>

    GetDefenceResourcesStartIndex : Ref< ^source> -> int
    GetAttackResourcesStartIndex  : Ref< ^source> -> int
    GetAttackResourcesNumber      : Ref< ^source> -> int

  }

type [<CasanovaEntity>] ResourcesTresholdProxy< ^SrcEntity, ^TgtEntity, ^world > = 
  {
      ResourceVectorSize  : int
      Name                : string
      DistMax             : float32<m>

      ActionStatus        : Var<Status> //done

      SourceEntity        : Ref< ^SrcEntity>
      SourceOutput        : Rule<SparseVector>

      LocalResource       : Rule<SparseVector> //done

      Target              : Ref< ^TgtEntity>
      TargetOutput        : Rule<SparseVector> //done

      GivenResources      : SparseMatrix
      TakenResources      : SparseMatrix

      ResourcesToSend     : SparseVector
      Treshold            : SparseVector

      Operations          : ProxyOperations< ^SrcEntity, ^TgtEntity, ^world>


  } with

  member inline this.IsDead =
    match !this.ActionStatus with    
    | Died -> true
    | _ when this.Operations.IsOwnerDead this.SourceEntity -> false
    | _ -> false

  member inline this.IsEnabled    = 
    match !this.ActionStatus with
    | Status.Automatic -> true
    | Status.Manual(enable) -> !enable.Enabled
    | Died -> false

  static member inline LocalResourceRule (world : ^world, 
                                          self  : ResourcesTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                          dt    : float32<s>) =   
    if not self.IsEnabled then !self.LocalResource
    else
      if self.TargetOutput.Value.Sum() = 0.0f && greater_or_equal self.Treshold !self.LocalResource then 

        let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
        let distance = Vector2<m>.Distance(self.Operations.GetSourcePos self.SourceEntity, self.Operations.GetTargetPos self.Target)

        if proxies > 0.0f && distance < self.DistMax then
          let source_resource = self.Operations.GetLocalResource self.SourceEntity
          let resources_to_split = source_resource / proxies
          let resources = SparseVector(self.GivenResources * resources_to_split )
          let dt_resources = resources * (float32 dt)
          let res = !self.LocalResource + dt_resources
          for i = 0 to res.Count - 1 do
            res.[i] <- min self.Treshold.[i] (max 0.0f res.[i])
          res
        else SparseVector(self.ResourceVectorSize)
      else SparseVector(self.ResourceVectorSize)

  static member inline TargetOutputRule (world : ^world, 
                                         self  : ResourcesTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                         dt    : float32<s>) =

    if greater_or_equal !self.LocalResource self.Treshold && self.IsEnabled then        
      self.ResourcesToSend
    else SparseVector(self.ResourceVectorSize)
      
  static member inline SourceOutputRule (world : ^world, 
                                         self  : ResourcesTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                         dt    : float32<s>) =
    if self.IsEnabled then
      let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
      let distance = Vector2<m>.Distance(self.Operations.GetSourcePos self.SourceEntity, self.Operations.GetTargetPos self.Target)
      if proxies > 0.0f && distance < self.DistMax then
        let source_resource = self.Operations.GetLocalResource self.SourceEntity
        let resources_to_split = source_resource / proxies
        let res = self.TakenResources * resources_to_split
        res * (float32 dt)
      else SparseVector(self.ResourceVectorSize)
    else SparseVector(self.ResourceVectorSize)

and [<CasanovaEntity>] ActionTresholdProxy< ^SrcEntity, ^TgtEntity, ^world> = 
  {
      ResourceVectorSize  : int
      Name                : string
      DistMax             : float32<m>

      ActionStatus        : Rule<Status> //done

      SourceEntity        : Ref< ^SrcEntity>
      SourceOutput        : Rule<SparseVector>

      LocalResource       : Rule<SparseVector> //done

      Target              : Ref< ^TgtEntity>
      TargetOutput        : Rule<SparseVector> 

      GivenResources      : SparseMatrix
      TakenResources      : SparseMatrix

      Treshold            : SparseVector

      Operations          : ProxyOperations< ^SrcEntity, ^TgtEntity, ^world>

      Action              : ^SrcEntity -> ^TgtEntity -> ^world -> Coroutine<unit>

      MakeAction          : Rule<Option<Coroutine<unit>>>

  } with

  member inline this.IsDead =
    match !this.ActionStatus with    
    | Died -> true
    | _ when this.Operations.IsOwnerDead this.SourceEntity -> true
    | _ -> false

  member inline this.IsEnabled    = 
    match !this.ActionStatus with
    | Status.Automatic -> true
    | Status.Manual(enable) -> !enable.Enabled
    | Died -> false

  static member inline MakeActionRule (world : ^world, 
                                       self  : ActionTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                       dt    : float32<s>) =
    if greater_or_equal !self.LocalResource self.Treshold  && self.IsEnabled then 
      let co = self.Action !self.SourceEntity !self.Target world
      Some co
    else None

  static member inline LocalResourceRule (world : ^world, 
                                          self  : ActionTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                          dt    : float32<s>) =
    if not self.IsEnabled then !self.LocalResource
    else
      if greater_or_equal self.Treshold !self.LocalResource then 
        let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
        if proxies > 0.0f then
          let source_resource = self.Operations.GetLocalResource self.SourceEntity

          let resources_to_split = source_resource / proxies   
          let resources = SparseVector(self.GivenResources * resources_to_split )
          let dt_resources = resources * (float32 dt)
          let res = !self.LocalResource + dt_resources

          for i = 0 to res.Count - 1 do
            res.[i] <- min self.Treshold.[i] (max 0.0f res.[i])

          res
        else SparseVector(self.ResourceVectorSize)
      else SparseVector(self.ResourceVectorSize)

  static member inline ActionStatusRule (world : ^world, 
                                         self  : ActionTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                         dt    : float32<s>) : Status =
    if greater_or_equal !self.LocalResource self.Treshold && self.IsEnabled then Status.Died
    else !self.ActionStatus

  static member inline SourceOutputRule (world : ^world, 
                                         self  : ActionTresholdProxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                         dt    : float32<s>) =
    if self.IsEnabled then
      let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
      if proxies > 0.f then
        let source_resource = self.Operations.GetLocalResource self.SourceEntity
        let resources_to_split = (source_resource / proxies)
            
        let resources = self.TakenResources * resources_to_split
        resources * (float32 dt)
      else SparseVector(self.ResourceVectorSize)
    else SparseVector(self.ResourceVectorSize)

type [<CasanovaEntity>] Proxy< ^SrcEntity, ^TgtEntity, ^world > = 
  {
      ResourceVectorSize  : int
      Name                : string
      DistMax             : float32<m>
      ActionStatus        : Var<Status>

      TargetsEntity       : RuleTable<Ref< ^TgtEntity>>
      TargetsOutput       : RuleTable<Ref< ^TgtEntity> * SparseVector>

      SourceEntity        : Ref< ^SrcEntity>
      SourceOutput        : Rule<SparseVector>

      GivenResources      : SparseMatrix
      TakenResources      : SparseMatrix

      Operations          : ProxyOperations< ^SrcEntity, ^TgtEntity, ^world>
  } with

  member inline this.IsEnabled    = 
    match !this.ActionStatus with
    | Status.Automatic -> true
    | Status.Manual(enable) -> !enable.Enabled
    | Died -> false

  member inline this.IsDead =
    match !this.ActionStatus with    
    | Died -> true
    | _ when this.Operations.IsOwnerDead this.SourceEntity -> false
    | _ -> false

  [<RuleUpdateFrequencyAttribute(UpdateFrequency.AI)>]  
  static member inline TargetsEntityRule (world : ^world, 
                                          self  : Proxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                          dt    : float32<s>) =
    let res = self.Operations.GetEntities self.SourceEntity self.DistMax (ref world)
    res

  static member inline SourceOutputRule (world : ^world, 
                                         self  : Proxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                         dt    : float32<s>) =    
    if self.IsEnabled then
      let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
      if proxies > 0.0f then
        let source_resource = self.Operations.GetLocalResource self.SourceEntity
        let resources_to_split = (source_resource / proxies)
        let resources = self.TakenResources * resources_to_split
        resources * (float32 dt)
      else SparseVector(self.ResourceVectorSize)
    else SparseVector(self.ResourceVectorSize)

  static member inline TargetsOutputRule (world : ^world, 
                                          self  : Proxy< ^SrcEntity, ^TgtEntity, ^world>, 
                                          dt    : float32<s>) =   
    
    if self.IsEnabled then
      let proxies = self.Operations.CountEnabledProxies self.SourceEntity (ref world) |> float32
      if proxies > 0.f then
        let source_resource = self.Operations.GetLocalResource self.SourceEntity * 1.0f
        let resources_to_split = source_resource / proxies    

        let defence_resources_start_index = self.Operations.GetDefenceResourcesStartIndex self.SourceEntity
        let attack_resources_start_index = self.Operations.GetAttackResourcesStartIndex self.SourceEntity
        let attack_resources_count = self.Operations.GetAttackResourcesNumber self.SourceEntity

        let row_index = attack_resources_start_index
        let row_count = attack_resources_count
        let column_index = defence_resources_start_index
        let column_count = self.GivenResources.ColumnCount - defence_resources_start_index

        let matrix' = SparseMatrix(self.GivenResources)
        let matrix'_sum = 
          let mutable sum = 0.0f
          for j = row_index to row_index + row_count - 1 do
            for i = column_index to column_index + column_count - 1 do
              let res = self.GivenResources.[i,j]
              sum <- sum + self.GivenResources.[i,j]
          sum

        seq{
          for target in !self.TargetsEntity do
            let ipi =
    
                let given_resources =
                  if matrix'_sum <> 0.f then
                    let local_res = self.Operations.GetTargetResource target

                    let sub_vector_sum = 
                      let mutable sum = 0.0f
                      for i = defence_resources_start_index to max (local_res.Count - 2) 1 do
                        sum <- sum + local_res.[i]
                      sum

                    if sub_vector_sum > 0.0f then
                      for i = 0 to matrix'.RowCount - 1 do
                        matrix'.[matrix'.ColumnCount - 1, i] <- 0.0f
                    matrix'
                  else self.GivenResources
              

                let ips_sum_target = SparseVector(given_resources * resources_to_split )
                ips_sum_target * (float32 dt)

            yield target, SparseVector(ipi)
          }
      else Seq.empty

    else Seq.empty

let internal set_value_of_matrix (matrix : Matrix) (_from : int) (_to : int) (value : float32) =  
  matrix.[_to,_from ] <- value

let copy_vector (_to : Vector) (_from: Vector) =  
  for i = 0 to _to.Count - 1 do
    _to.[i] <- _from.[i]

let copy_matrix (_to : Matrix) (_from: Matrix) =  
  for i = 0 to _to.ColumnCount - 1 do
    for j = 0 to _to.RowCount - 1 do
      _to.[i, j] <- _from.[i, j]

let internal set_value_of_vector (matrix : Vector) (i : int) (value : float32) =
  matrix.[i] <- value

let inline make_continuous_proxy (source : 'a) (operations : ProxyOperations<'a,'b,'c>) info dist_max status resources_number : Proxy<'a,'b,'c> =  
  {
    ResourceVectorSize  = resources_number
    Name                = info
    DistMax             = dist_max
    ActionStatus        = var  status

    TargetsEntity       = RuleTable.Create(fun () -> Seq.empty)
    TargetsOutput       = RuleTable.Create(fun () -> Seq.empty)

    SourceEntity        = ref source
    SourceOutput        = Rule.Create(SparseVector(resources_number))

    GivenResources      = 
      let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
      SparseMatrix(array)
    TakenResources      =
      let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
      SparseMatrix(array)

    Operations          = operations
  }

let inline make_treshold_proxy
    (source : 'a) 
    (target : 'b) 
    treshold
    resourcesToSend
    (operations : ProxyOperations<'a,'b,'c>) 
    info 
    dist_max 
    status 
    resources_number : ResourcesTresholdProxy<'a,'b,'c> =

  {
        ResourceVectorSize  = resources_number
        Name                = info
        DistMax             = dist_max
        ActionStatus        = var status
        SourceEntity        = ref source
        SourceOutput        = Rule.Create(SparseVector(resources_number))

        LocalResource       = Rule.Create(SparseVector(resources_number))

        Target              = ref target
        TargetOutput        = Rule.Create(SparseVector(resources_number))

        GivenResources      = 
          let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
          SparseMatrix(array)
        TakenResources      =
          let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
          SparseMatrix(array)

        ResourcesToSend     = resourcesToSend
        Treshold            = treshold

        Operations          = operations
    }

let inline make_treshold_proxy_for_action
    (source : 'a) 
    (target : 'b) 
    treshold
    (operations : ProxyOperations<'a,'b,'c>) 
    info 
    dist_max 
    status 
    action
    resources_number : ActionTresholdProxy<'a,'b,'c> =
  {
        ResourceVectorSize  = resources_number
        Name                = info
        DistMax             = dist_max

        ActionStatus        = Rule.Create(fun () -> status)

        SourceEntity        = ref source
        SourceOutput        = Rule.Create(SparseVector(resources_number))

        LocalResource       = Rule.Create(SparseVector(resources_number))

        Target              = ref target
        TargetOutput        = Rule.Create(SparseVector(resources_number))

        GivenResources      = let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
                              SparseMatrix(array)
        TakenResources      = let array = Array2D.init resources_number resources_number (fun n m -> 0.0f)
                              SparseMatrix(array)

        Treshold            = treshold

        Operations          = operations

        Action              = action

        MakeAction          = Rule.Create(None)

    }

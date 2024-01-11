[ PushArg 0
, Push (VInt 0)
, Push (VOp Less)
, Call
, JumpIfFalse 2
, PushArg 0
, Ret
, PushArg 0
, Push (VInt (-1))
, Push (VOp Mul)
, Call
, Ret
]
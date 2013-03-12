  [EXP_OP (OP_EQUAL,[EXP_VAR "x0",EXP_INT 1]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x1",EXP_INT 2]),
   EXP_OP
     (OP_EQUAL,[EXP_VAR "x2",EXP_OP (OP_SET,[EXP_VAR "x0",EXP_VAR "x1"])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x3",
       EXP_OP
         (OP_SET,
          [EXP_OP (OP_TUPLE,[EXP_INT 1,EXP_INT 2]),
           EXP_OP (OP_TUPLE,[EXP_INT 3,EXP_INT 4])])]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x4",EXP_OP (OP_IS_FUNCTION,[EXP_VAR "x2"])]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x5",EXP_OP (OP_IS_FUNCTION,[EXP_VAR "x3"])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x6",
       EXP_OP
         (OP_EQUAL,[EXP_VAR "x2",EXP_OP (OP_SET,[EXP_VAR "x0",EXP_INT 2])])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x7",EXP_OP (OP_APPLY_FUNCTION,[EXP_VAR "x3",EXP_VAR "x0"])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x8",
       EXP_OP
         (OP_TUPLE,
          [EXP_OP (OP_TUPLE,[EXP_INT 0,EXP_INT 1]),
           EXP_OP
             (OP_TUPLE,[EXP_INT 3,EXP_OP (OP_TUPLE,[EXP_INT 4,EXP_INT 5])])])]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x9",EXP_OP (OP_SET,[EXP_VAR "x8"])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x10",
       EXP_OP
         (OP_SET,
          [EXP_OP (OP_TUPLE,[EXP_INT 0,EXP_INT 4]),
           EXP_OP (OP_TUPLE,[EXP_INT 1,EXP_INT 6]),
           EXP_OP (OP_TUPLE,[EXP_INT 2,EXP_INT 1])])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x11",EXP_OP (OP_APPLY_FUNCTION,[EXP_VAR "x3",EXP_VAR "x1"])]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x12",EXP_INT ~1]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x13",EXP_OP (OP_MEMBER,[EXP_VAR "x12",EXP_VAR "x2"])]),
   EXP_OP
     (OP_EQUAL,[EXP_VAR "x14",EXP_OP (OP_MEMBER,[EXP_VAR "x0",EXP_VAR "x2"])]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x15",
       EXP_OP
         (OP_EQUAL,
          [EXP_VAR "x2",
           EXP_OP (OP_SET,[EXP_VAR "x0",EXP_VAR "x1",EXP_VAR "x1"])])]),
   EXP_OP (OP_EQUAL,[EXP_VAR "x16",EXP_VAR "x3"]),
   EXP_OP
     (OP_EQUAL,
      [EXP_VAR "x17",
       EXP_OP
         (OP_APPLY_FUNCTION,
          [EXP_VAR "x9",
           EXP_OP
             (OP_TUPLE,
              [EXP_INT 0,EXP_OP (OP_APPLY_FUNCTION,[EXP_VAR "x10",EXP_INT 2])])])])]
 
{-# OPTIONS_GHC -w #-}
--------------------------------------------------------------------------------
-- (c) 2013 Kwanghoon Choi
-- kwanghoon.choi@yonsei.ac.kr

-- A simple parser for a subset of Java:
--------------------------------------------------------------------------------

module Parser where

import Data.Char
import Data.List
import AST

dummy_var_name = "$d"

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39

action_0 (62) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (62) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (63) = happyShift action_12
action_2 (8) = happyGoto action_9
action_2 (9) = happyGoto action_10
action_2 (10) = happyGoto action_11
action_2 _ = happyReduce_8

action_3 _ = happyReduce_3

action_4 (78) = happyShift action_7
action_4 (79) = happyShift action_8
action_4 (7) = happyGoto action_6
action_4 _ = happyFail

action_5 (86) = happyAccept
action_5 _ = happyFail

action_6 (40) = happyShift action_23
action_6 _ = happyFail

action_7 (41) = happyShift action_22
action_7 _ = happyReduce_5

action_8 _ = happyReduce_6

action_9 (64) = happyShift action_19
action_9 (65) = happyShift action_20
action_9 (66) = happyShift action_21
action_9 (11) = happyGoto action_15
action_9 (25) = happyGoto action_16
action_9 (35) = happyGoto action_17
action_9 (36) = happyGoto action_18
action_9 _ = happyReduce_98

action_10 (63) = happyShift action_12
action_10 (9) = happyGoto action_10
action_10 (10) = happyGoto action_14
action_10 _ = happyReduce_11

action_11 _ = happyReduce_9

action_12 (78) = happyShift action_7
action_12 (79) = happyShift action_8
action_12 (7) = happyGoto action_13
action_12 _ = happyFail

action_13 (40) = happyShift action_29
action_13 _ = happyFail

action_14 _ = happyReduce_12

action_15 (64) = happyShift action_19
action_15 (65) = happyShift action_20
action_15 (66) = happyShift action_21
action_15 (86) = happyReduce_71
action_15 (11) = happyGoto action_15
action_15 (25) = happyGoto action_28
action_15 (35) = happyGoto action_17
action_15 (36) = happyGoto action_18
action_15 _ = happyReduce_98

action_16 _ = happyReduce_1

action_17 (68) = happyShift action_26
action_17 (69) = happyShift action_27
action_17 (37) = happyGoto action_25
action_17 _ = happyReduce_103

action_18 _ = happyReduce_99

action_19 _ = happyReduce_100

action_20 _ = happyReduce_101

action_21 _ = happyReduce_102

action_22 (78) = happyShift action_7
action_22 (79) = happyShift action_8
action_22 (7) = happyGoto action_24
action_22 _ = happyFail

action_23 _ = happyReduce_4

action_24 _ = happyReduce_7

action_25 (67) = happyShift action_32
action_25 (38) = happyGoto action_31
action_25 _ = happyReduce_105

action_26 _ = happyReduce_104

action_27 (79) = happyShift action_30
action_27 _ = happyFail

action_28 _ = happyReduce_72

action_29 _ = happyReduce_10

action_30 (55) = happyShift action_35
action_30 (12) = happyGoto action_34
action_30 _ = happyReduce_15

action_31 (54) = happyShift action_33
action_31 _ = happyFail

action_32 _ = happyReduce_106

action_33 (79) = happyShift action_38
action_33 _ = happyFail

action_34 (45) = happyShift action_37
action_34 _ = happyFail

action_35 (79) = happyShift action_36
action_35 _ = happyFail

action_36 _ = happyReduce_16

action_37 (64) = happyShift action_19
action_37 (65) = happyShift action_20
action_37 (66) = happyShift action_21
action_37 (71) = happyReduce_98
action_37 (79) = happyReduce_98
action_37 (80) = happyReduce_98
action_37 (27) = happyGoto action_40
action_37 (35) = happyGoto action_41
action_37 (36) = happyGoto action_18
action_37 _ = happyReduce_77

action_38 (55) = happyShift action_35
action_38 (12) = happyGoto action_39
action_38 _ = happyReduce_15

action_39 (70) = happyShift action_46
action_39 (13) = happyGoto action_45
action_39 _ = happyReduce_17

action_40 (46) = happyShift action_44
action_40 _ = happyFail

action_41 (71) = happyShift action_43
action_41 (39) = happyGoto action_42
action_41 _ = happyReduce_107

action_42 (79) = happyShift action_53
action_42 (80) = happyShift action_54
action_42 (17) = happyGoto action_50
action_42 (33) = happyGoto action_51
action_42 (34) = happyGoto action_52
action_42 _ = happyFail

action_43 _ = happyReduce_108

action_44 _ = happyReduce_14

action_45 (45) = happyShift action_49
action_45 _ = happyFail

action_46 (79) = happyShift action_48
action_46 (14) = happyGoto action_47
action_46 _ = happyFail

action_47 _ = happyReduce_18

action_48 (42) = happyShift action_60
action_48 _ = happyReduce_19

action_49 (64) = happyShift action_19
action_49 (65) = happyShift action_20
action_49 (66) = happyShift action_21
action_49 (67) = happyReduce_98
action_49 (71) = happyReduce_98
action_49 (79) = happyReduce_98
action_49 (80) = happyReduce_98
action_49 (26) = happyGoto action_58
action_49 (35) = happyGoto action_59
action_49 (36) = happyGoto action_18
action_49 _ = happyReduce_73

action_50 (64) = happyShift action_19
action_50 (65) = happyShift action_20
action_50 (66) = happyShift action_21
action_50 (71) = happyReduce_98
action_50 (79) = happyReduce_98
action_50 (80) = happyReduce_98
action_50 (27) = happyGoto action_57
action_50 (35) = happyGoto action_41
action_50 (36) = happyGoto action_18
action_50 _ = happyReduce_77

action_51 (47) = happyShift action_55
action_51 (78) = happyShift action_56
action_51 _ = happyFail

action_52 _ = happyReduce_94

action_53 _ = happyReduce_96

action_54 _ = happyReduce_97

action_55 (48) = happyShift action_65
action_55 _ = happyFail

action_56 (43) = happyShift action_64
action_56 _ = happyFail

action_57 _ = happyReduce_78

action_58 (46) = happyShift action_63
action_58 _ = happyFail

action_59 (71) = happyShift action_43
action_59 (39) = happyGoto action_62
action_59 _ = happyReduce_107

action_60 (79) = happyShift action_48
action_60 (14) = happyGoto action_61
action_60 _ = happyFail

action_61 _ = happyReduce_20

action_62 (67) = happyShift action_32
action_62 (38) = happyGoto action_69
action_62 _ = happyReduce_105

action_63 _ = happyReduce_13

action_64 (71) = happyShift action_43
action_64 (79) = happyReduce_107
action_64 (80) = happyReduce_107
action_64 (28) = happyGoto action_66
action_64 (29) = happyGoto action_67
action_64 (39) = happyGoto action_68
action_64 _ = happyReduce_79

action_65 _ = happyReduce_95

action_66 (44) = happyShift action_76
action_66 _ = happyFail

action_67 _ = happyReduce_80

action_68 (79) = happyShift action_53
action_68 (80) = happyShift action_54
action_68 (33) = happyGoto action_75
action_68 (34) = happyGoto action_52
action_68 _ = happyFail

action_69 (79) = happyShift action_74
action_69 (80) = happyShift action_54
action_69 (15) = happyGoto action_70
action_69 (16) = happyGoto action_71
action_69 (18) = happyGoto action_72
action_69 (33) = happyGoto action_73
action_69 (34) = happyGoto action_52
action_69 _ = happyFail

action_70 (64) = happyShift action_19
action_70 (65) = happyShift action_20
action_70 (66) = happyShift action_21
action_70 (67) = happyReduce_98
action_70 (71) = happyReduce_98
action_70 (79) = happyReduce_98
action_70 (80) = happyReduce_98
action_70 (26) = happyGoto action_83
action_70 (35) = happyGoto action_59
action_70 (36) = happyGoto action_18
action_70 _ = happyReduce_73

action_71 (64) = happyShift action_19
action_71 (65) = happyShift action_20
action_71 (66) = happyShift action_21
action_71 (67) = happyReduce_98
action_71 (71) = happyReduce_98
action_71 (79) = happyReduce_98
action_71 (80) = happyReduce_98
action_71 (26) = happyGoto action_82
action_71 (35) = happyGoto action_59
action_71 (36) = happyGoto action_18
action_71 _ = happyReduce_73

action_72 (64) = happyShift action_19
action_72 (65) = happyShift action_20
action_72 (66) = happyShift action_21
action_72 (67) = happyReduce_98
action_72 (71) = happyReduce_98
action_72 (79) = happyReduce_98
action_72 (80) = happyReduce_98
action_72 (26) = happyGoto action_81
action_72 (35) = happyGoto action_59
action_72 (36) = happyGoto action_18
action_72 _ = happyReduce_73

action_73 (47) = happyShift action_55
action_73 (78) = happyShift action_80
action_73 _ = happyFail

action_74 (43) = happyShift action_79
action_74 _ = happyReduce_96

action_75 (47) = happyShift action_55
action_75 (78) = happyShift action_78
action_75 _ = happyFail

action_76 (40) = happyShift action_77
action_76 _ = happyFail

action_77 _ = happyReduce_24

action_78 (42) = happyShift action_88
action_78 _ = happyReduce_81

action_79 (71) = happyShift action_43
action_79 (79) = happyReduce_107
action_79 (80) = happyReduce_107
action_79 (28) = happyGoto action_87
action_79 (29) = happyGoto action_67
action_79 (39) = happyGoto action_68
action_79 _ = happyReduce_79

action_80 (40) = happyShift action_84
action_80 (43) = happyShift action_85
action_80 (49) = happyShift action_86
action_80 _ = happyFail

action_81 _ = happyReduce_76

action_82 _ = happyReduce_75

action_83 _ = happyReduce_74

action_84 _ = happyReduce_21

action_85 (71) = happyShift action_43
action_85 (79) = happyReduce_107
action_85 (80) = happyReduce_107
action_85 (28) = happyGoto action_105
action_85 (29) = happyGoto action_67
action_85 (39) = happyGoto action_68
action_85 _ = happyReduce_79

action_86 (43) = happyShift action_96
action_86 (56) = happyShift action_97
action_86 (59) = happyShift action_98
action_86 (60) = happyShift action_99
action_86 (74) = happyShift action_100
action_86 (75) = happyShift action_101
action_86 (76) = happyShift action_102
action_86 (77) = happyShift action_103
action_86 (78) = happyShift action_104
action_86 (79) = happyShift action_53
action_86 (80) = happyShift action_54
action_86 (19) = happyGoto action_91
action_86 (20) = happyGoto action_92
action_86 (21) = happyGoto action_93
action_86 (32) = happyGoto action_94
action_86 (34) = happyGoto action_95
action_86 _ = happyFail

action_87 (44) = happyShift action_90
action_87 _ = happyFail

action_88 (71) = happyShift action_43
action_88 (29) = happyGoto action_89
action_88 (39) = happyGoto action_68
action_88 _ = happyReduce_107

action_89 _ = happyReduce_82

action_90 (45) = happyShift action_126
action_90 _ = happyFail

action_91 (40) = happyShift action_125
action_91 _ = happyFail

action_92 _ = happyReduce_32

action_93 (41) = happyShift action_122
action_93 (47) = happyShift action_123
action_93 (49) = happyShift action_124
action_93 (50) = happyReduce_87
action_93 (51) = happyReduce_87
action_93 (52) = happyReduce_87
action_93 (53) = happyReduce_87
action_93 (81) = happyReduce_87
action_93 (82) = happyReduce_87
action_93 (83) = happyReduce_87
action_93 (84) = happyReduce_87
action_93 (85) = happyReduce_87
action_93 _ = happyReduce_33

action_94 (50) = happyShift action_113
action_94 (51) = happyShift action_114
action_94 (52) = happyShift action_115
action_94 (53) = happyShift action_116
action_94 (81) = happyShift action_117
action_94 (82) = happyShift action_118
action_94 (83) = happyShift action_119
action_94 (84) = happyShift action_120
action_94 (85) = happyShift action_121
action_94 _ = happyFail

action_95 (41) = happyShift action_112
action_95 _ = happyFail

action_96 (43) = happyShift action_96
action_96 (56) = happyShift action_97
action_96 (59) = happyShift action_98
action_96 (60) = happyShift action_99
action_96 (74) = happyShift action_100
action_96 (75) = happyShift action_101
action_96 (76) = happyShift action_102
action_96 (77) = happyShift action_103
action_96 (78) = happyShift action_104
action_96 (79) = happyShift action_53
action_96 (80) = happyShift action_54
action_96 (19) = happyGoto action_109
action_96 (20) = happyGoto action_92
action_96 (21) = happyGoto action_93
action_96 (32) = happyGoto action_94
action_96 (33) = happyGoto action_110
action_96 (34) = happyGoto action_111
action_96 _ = happyFail

action_97 (79) = happyShift action_53
action_97 (80) = happyShift action_54
action_97 (34) = happyGoto action_108
action_97 _ = happyFail

action_98 (50) = happyReduce_91
action_98 (51) = happyReduce_91
action_98 (52) = happyReduce_91
action_98 (53) = happyReduce_91
action_98 (81) = happyReduce_91
action_98 (82) = happyReduce_91
action_98 (83) = happyReduce_91
action_98 (84) = happyReduce_91
action_98 (85) = happyReduce_91
action_98 _ = happyReduce_34

action_99 (50) = happyReduce_92
action_99 (51) = happyReduce_92
action_99 (52) = happyReduce_92
action_99 (53) = happyReduce_92
action_99 (81) = happyReduce_92
action_99 (82) = happyReduce_92
action_99 (83) = happyReduce_92
action_99 (84) = happyReduce_92
action_99 (85) = happyReduce_92
action_99 _ = happyReduce_35

action_100 (50) = happyReduce_93
action_100 (51) = happyReduce_93
action_100 (52) = happyReduce_93
action_100 (53) = happyReduce_93
action_100 (81) = happyReduce_93
action_100 (82) = happyReduce_93
action_100 (83) = happyReduce_93
action_100 (84) = happyReduce_93
action_100 (85) = happyReduce_93
action_100 _ = happyReduce_36

action_101 (50) = happyReduce_90
action_101 (51) = happyReduce_90
action_101 (52) = happyReduce_90
action_101 (53) = happyReduce_90
action_101 (81) = happyReduce_90
action_101 (82) = happyReduce_90
action_101 (83) = happyReduce_90
action_101 (84) = happyReduce_90
action_101 (85) = happyReduce_90
action_101 _ = happyReduce_37

action_102 (50) = happyReduce_88
action_102 (51) = happyReduce_88
action_102 (52) = happyReduce_88
action_102 (53) = happyReduce_88
action_102 (81) = happyReduce_88
action_102 (82) = happyReduce_88
action_102 (83) = happyReduce_88
action_102 (84) = happyReduce_88
action_102 (85) = happyReduce_88
action_102 _ = happyReduce_38

action_103 (50) = happyReduce_89
action_103 (51) = happyReduce_89
action_103 (52) = happyReduce_89
action_103 (53) = happyReduce_89
action_103 (81) = happyReduce_89
action_103 (82) = happyReduce_89
action_103 (83) = happyReduce_89
action_103 (84) = happyReduce_89
action_103 (85) = happyReduce_89
action_103 _ = happyReduce_39

action_104 (43) = happyShift action_107
action_104 _ = happyReduce_49

action_105 (44) = happyShift action_106
action_105 _ = happyFail

action_106 (45) = happyShift action_165
action_106 _ = happyFail

action_107 (43) = happyShift action_96
action_107 (56) = happyShift action_97
action_107 (59) = happyShift action_98
action_107 (60) = happyShift action_99
action_107 (74) = happyShift action_100
action_107 (75) = happyShift action_101
action_107 (76) = happyShift action_102
action_107 (77) = happyShift action_103
action_107 (78) = happyShift action_104
action_107 (79) = happyShift action_53
action_107 (80) = happyShift action_54
action_107 (19) = happyGoto action_162
action_107 (20) = happyGoto action_92
action_107 (21) = happyGoto action_93
action_107 (30) = happyGoto action_163
action_107 (31) = happyGoto action_164
action_107 (32) = happyGoto action_94
action_107 (34) = happyGoto action_95
action_107 _ = happyReduce_83

action_108 (43) = happyShift action_160
action_108 (47) = happyShift action_161
action_108 _ = happyFail

action_109 (44) = happyShift action_159
action_109 _ = happyFail

action_110 (44) = happyShift action_158
action_110 (47) = happyShift action_55
action_110 _ = happyFail

action_111 (41) = happyShift action_112
action_111 _ = happyReduce_94

action_112 (78) = happyShift action_157
action_112 _ = happyFail

action_113 (59) = happyShift action_144
action_113 (60) = happyShift action_145
action_113 (74) = happyShift action_146
action_113 (75) = happyShift action_147
action_113 (76) = happyShift action_148
action_113 (77) = happyShift action_149
action_113 (78) = happyShift action_150
action_113 (79) = happyShift action_53
action_113 (80) = happyShift action_54
action_113 (21) = happyGoto action_142
action_113 (32) = happyGoto action_156
action_113 (34) = happyGoto action_95
action_113 _ = happyFail

action_114 (59) = happyShift action_144
action_114 (60) = happyShift action_145
action_114 (74) = happyShift action_146
action_114 (75) = happyShift action_147
action_114 (76) = happyShift action_148
action_114 (77) = happyShift action_149
action_114 (78) = happyShift action_150
action_114 (79) = happyShift action_53
action_114 (80) = happyShift action_54
action_114 (21) = happyGoto action_142
action_114 (32) = happyGoto action_155
action_114 (34) = happyGoto action_95
action_114 _ = happyFail

action_115 (59) = happyShift action_144
action_115 (60) = happyShift action_145
action_115 (74) = happyShift action_146
action_115 (75) = happyShift action_147
action_115 (76) = happyShift action_148
action_115 (77) = happyShift action_149
action_115 (78) = happyShift action_150
action_115 (79) = happyShift action_53
action_115 (80) = happyShift action_54
action_115 (21) = happyGoto action_142
action_115 (32) = happyGoto action_154
action_115 (34) = happyGoto action_95
action_115 _ = happyFail

action_116 (59) = happyShift action_144
action_116 (60) = happyShift action_145
action_116 (74) = happyShift action_146
action_116 (75) = happyShift action_147
action_116 (76) = happyShift action_148
action_116 (77) = happyShift action_149
action_116 (78) = happyShift action_150
action_116 (79) = happyShift action_53
action_116 (80) = happyShift action_54
action_116 (21) = happyGoto action_142
action_116 (32) = happyGoto action_153
action_116 (34) = happyGoto action_95
action_116 _ = happyFail

action_117 (59) = happyShift action_144
action_117 (60) = happyShift action_145
action_117 (74) = happyShift action_146
action_117 (75) = happyShift action_147
action_117 (76) = happyShift action_148
action_117 (77) = happyShift action_149
action_117 (78) = happyShift action_150
action_117 (79) = happyShift action_53
action_117 (80) = happyShift action_54
action_117 (21) = happyGoto action_142
action_117 (32) = happyGoto action_152
action_117 (34) = happyGoto action_95
action_117 _ = happyFail

action_118 (59) = happyShift action_144
action_118 (60) = happyShift action_145
action_118 (74) = happyShift action_146
action_118 (75) = happyShift action_147
action_118 (76) = happyShift action_148
action_118 (77) = happyShift action_149
action_118 (78) = happyShift action_150
action_118 (79) = happyShift action_53
action_118 (80) = happyShift action_54
action_118 (21) = happyGoto action_142
action_118 (32) = happyGoto action_151
action_118 (34) = happyGoto action_95
action_118 _ = happyFail

action_119 (59) = happyShift action_144
action_119 (60) = happyShift action_145
action_119 (74) = happyShift action_146
action_119 (75) = happyShift action_147
action_119 (76) = happyShift action_148
action_119 (77) = happyShift action_149
action_119 (78) = happyShift action_150
action_119 (79) = happyShift action_53
action_119 (80) = happyShift action_54
action_119 (21) = happyGoto action_142
action_119 (32) = happyGoto action_143
action_119 (34) = happyGoto action_95
action_119 _ = happyFail

action_120 _ = happyReduce_47

action_121 _ = happyReduce_48

action_122 (78) = happyShift action_141
action_122 _ = happyFail

action_123 (43) = happyShift action_96
action_123 (56) = happyShift action_97
action_123 (59) = happyShift action_98
action_123 (60) = happyShift action_99
action_123 (74) = happyShift action_100
action_123 (75) = happyShift action_101
action_123 (76) = happyShift action_102
action_123 (77) = happyShift action_103
action_123 (78) = happyShift action_104
action_123 (79) = happyShift action_53
action_123 (80) = happyShift action_54
action_123 (19) = happyGoto action_140
action_123 (20) = happyGoto action_92
action_123 (21) = happyGoto action_93
action_123 (32) = happyGoto action_94
action_123 (34) = happyGoto action_95
action_123 _ = happyFail

action_124 (43) = happyShift action_96
action_124 (56) = happyShift action_97
action_124 (59) = happyShift action_98
action_124 (60) = happyShift action_99
action_124 (74) = happyShift action_100
action_124 (75) = happyShift action_101
action_124 (76) = happyShift action_102
action_124 (77) = happyShift action_103
action_124 (78) = happyShift action_104
action_124 (79) = happyShift action_53
action_124 (80) = happyShift action_54
action_124 (19) = happyGoto action_139
action_124 (20) = happyGoto action_92
action_124 (21) = happyGoto action_93
action_124 (32) = happyGoto action_94
action_124 (34) = happyGoto action_95
action_124 _ = happyFail

action_125 _ = happyReduce_22

action_126 (40) = happyShift action_132
action_126 (43) = happyShift action_96
action_126 (45) = happyShift action_133
action_126 (56) = happyShift action_97
action_126 (57) = happyShift action_134
action_126 (59) = happyShift action_98
action_126 (60) = happyShift action_99
action_126 (61) = happyShift action_135
action_126 (71) = happyShift action_136
action_126 (72) = happyShift action_137
action_126 (73) = happyShift action_138
action_126 (74) = happyShift action_100
action_126 (75) = happyShift action_101
action_126 (76) = happyShift action_102
action_126 (77) = happyShift action_103
action_126 (78) = happyShift action_104
action_126 (79) = happyShift action_53
action_126 (80) = happyShift action_54
action_126 (19) = happyGoto action_127
action_126 (20) = happyGoto action_92
action_126 (21) = happyGoto action_93
action_126 (22) = happyGoto action_128
action_126 (23) = happyGoto action_129
action_126 (24) = happyGoto action_130
action_126 (32) = happyGoto action_94
action_126 (33) = happyGoto action_131
action_126 (34) = happyGoto action_111
action_126 _ = happyReduce_55

action_127 (40) = happyShift action_185
action_127 _ = happyFail

action_128 (46) = happyShift action_184
action_128 _ = happyFail

action_129 _ = happyReduce_70

action_130 (40) = happyShift action_132
action_130 (43) = happyShift action_96
action_130 (45) = happyShift action_133
action_130 (56) = happyShift action_97
action_130 (57) = happyShift action_134
action_130 (59) = happyShift action_98
action_130 (60) = happyShift action_99
action_130 (61) = happyShift action_135
action_130 (71) = happyShift action_136
action_130 (72) = happyShift action_137
action_130 (73) = happyShift action_138
action_130 (74) = happyShift action_100
action_130 (75) = happyShift action_101
action_130 (76) = happyShift action_102
action_130 (77) = happyShift action_103
action_130 (78) = happyShift action_104
action_130 (79) = happyShift action_53
action_130 (80) = happyShift action_54
action_130 (19) = happyGoto action_127
action_130 (20) = happyGoto action_92
action_130 (21) = happyGoto action_93
action_130 (22) = happyGoto action_183
action_130 (23) = happyGoto action_129
action_130 (24) = happyGoto action_130
action_130 (32) = happyGoto action_94
action_130 (33) = happyGoto action_131
action_130 (34) = happyGoto action_111
action_130 _ = happyReduce_55

action_131 (47) = happyShift action_55
action_131 (78) = happyShift action_182
action_131 _ = happyFail

action_132 _ = happyReduce_69

action_133 (40) = happyShift action_132
action_133 (43) = happyShift action_96
action_133 (45) = happyShift action_133
action_133 (56) = happyShift action_97
action_133 (57) = happyShift action_134
action_133 (59) = happyShift action_98
action_133 (60) = happyShift action_99
action_133 (61) = happyShift action_135
action_133 (71) = happyShift action_136
action_133 (72) = happyShift action_137
action_133 (73) = happyShift action_138
action_133 (74) = happyShift action_100
action_133 (75) = happyShift action_101
action_133 (76) = happyShift action_102
action_133 (77) = happyShift action_103
action_133 (78) = happyShift action_104
action_133 (79) = happyShift action_53
action_133 (80) = happyShift action_54
action_133 (19) = happyGoto action_127
action_133 (20) = happyGoto action_92
action_133 (21) = happyGoto action_93
action_133 (22) = happyGoto action_181
action_133 (23) = happyGoto action_129
action_133 (24) = happyGoto action_130
action_133 (32) = happyGoto action_94
action_133 (33) = happyGoto action_131
action_133 (34) = happyGoto action_111
action_133 _ = happyReduce_55

action_134 (43) = happyShift action_180
action_134 _ = happyFail

action_135 (40) = happyShift action_179
action_135 (43) = happyShift action_96
action_135 (56) = happyShift action_97
action_135 (59) = happyShift action_98
action_135 (60) = happyShift action_99
action_135 (74) = happyShift action_100
action_135 (75) = happyShift action_101
action_135 (76) = happyShift action_102
action_135 (77) = happyShift action_103
action_135 (78) = happyShift action_104
action_135 (79) = happyShift action_53
action_135 (80) = happyShift action_54
action_135 (19) = happyGoto action_178
action_135 (20) = happyGoto action_92
action_135 (21) = happyGoto action_93
action_135 (32) = happyGoto action_94
action_135 (34) = happyGoto action_95
action_135 _ = happyFail

action_136 (79) = happyShift action_53
action_136 (80) = happyShift action_54
action_136 (33) = happyGoto action_177
action_136 (34) = happyGoto action_52
action_136 _ = happyFail

action_137 (43) = happyShift action_176
action_137 _ = happyFail

action_138 (43) = happyShift action_175
action_138 _ = happyFail

action_139 _ = happyReduce_44

action_140 (48) = happyShift action_174
action_140 _ = happyFail

action_141 (43) = happyShift action_173
action_141 _ = happyReduce_50

action_142 (41) = happyShift action_122
action_142 (47) = happyShift action_123
action_142 _ = happyReduce_87

action_143 _ = happyReduce_46

action_144 _ = happyReduce_91

action_145 _ = happyReduce_92

action_146 _ = happyReduce_93

action_147 _ = happyReduce_90

action_148 _ = happyReduce_88

action_149 _ = happyReduce_89

action_150 _ = happyReduce_49

action_151 _ = happyReduce_45

action_152 _ = happyReduce_27

action_153 _ = happyReduce_31

action_154 _ = happyReduce_30

action_155 _ = happyReduce_29

action_156 _ = happyReduce_28

action_157 (43) = happyShift action_172
action_157 _ = happyReduce_53

action_158 (43) = happyShift action_96
action_158 (56) = happyShift action_97
action_158 (59) = happyShift action_98
action_158 (60) = happyShift action_99
action_158 (74) = happyShift action_100
action_158 (75) = happyShift action_101
action_158 (76) = happyShift action_102
action_158 (77) = happyShift action_103
action_158 (78) = happyShift action_104
action_158 (79) = happyShift action_53
action_158 (80) = happyShift action_54
action_158 (19) = happyGoto action_171
action_158 (20) = happyGoto action_92
action_158 (21) = happyGoto action_93
action_158 (32) = happyGoto action_94
action_158 (34) = happyGoto action_95
action_158 _ = happyFail

action_159 _ = happyReduce_42

action_160 (43) = happyShift action_96
action_160 (56) = happyShift action_97
action_160 (59) = happyShift action_98
action_160 (60) = happyShift action_99
action_160 (74) = happyShift action_100
action_160 (75) = happyShift action_101
action_160 (76) = happyShift action_102
action_160 (77) = happyShift action_103
action_160 (78) = happyShift action_104
action_160 (79) = happyShift action_53
action_160 (80) = happyShift action_54
action_160 (19) = happyGoto action_162
action_160 (20) = happyGoto action_92
action_160 (21) = happyGoto action_93
action_160 (30) = happyGoto action_170
action_160 (31) = happyGoto action_164
action_160 (32) = happyGoto action_94
action_160 (34) = happyGoto action_95
action_160 _ = happyReduce_83

action_161 (43) = happyShift action_96
action_161 (56) = happyShift action_97
action_161 (59) = happyShift action_98
action_161 (60) = happyShift action_99
action_161 (74) = happyShift action_100
action_161 (75) = happyShift action_101
action_161 (76) = happyShift action_102
action_161 (77) = happyShift action_103
action_161 (78) = happyShift action_104
action_161 (79) = happyShift action_53
action_161 (80) = happyShift action_54
action_161 (19) = happyGoto action_162
action_161 (20) = happyGoto action_92
action_161 (21) = happyGoto action_93
action_161 (30) = happyGoto action_169
action_161 (31) = happyGoto action_164
action_161 (32) = happyGoto action_94
action_161 (34) = happyGoto action_95
action_161 _ = happyReduce_83

action_162 (42) = happyShift action_168
action_162 _ = happyReduce_85

action_163 (44) = happyShift action_167
action_163 _ = happyFail

action_164 _ = happyReduce_84

action_165 (40) = happyShift action_132
action_165 (43) = happyShift action_96
action_165 (45) = happyShift action_133
action_165 (56) = happyShift action_97
action_165 (57) = happyShift action_134
action_165 (59) = happyShift action_98
action_165 (60) = happyShift action_99
action_165 (61) = happyShift action_135
action_165 (71) = happyShift action_136
action_165 (72) = happyShift action_137
action_165 (73) = happyShift action_138
action_165 (74) = happyShift action_100
action_165 (75) = happyShift action_101
action_165 (76) = happyShift action_102
action_165 (77) = happyShift action_103
action_165 (78) = happyShift action_104
action_165 (79) = happyShift action_53
action_165 (80) = happyShift action_54
action_165 (19) = happyGoto action_127
action_165 (20) = happyGoto action_92
action_165 (21) = happyGoto action_93
action_165 (22) = happyGoto action_166
action_165 (23) = happyGoto action_129
action_165 (24) = happyGoto action_130
action_165 (32) = happyGoto action_94
action_165 (33) = happyGoto action_131
action_165 (34) = happyGoto action_111
action_165 _ = happyReduce_55

action_166 (46) = happyShift action_200
action_166 _ = happyFail

action_167 _ = happyReduce_26

action_168 (43) = happyShift action_96
action_168 (56) = happyShift action_97
action_168 (59) = happyShift action_98
action_168 (60) = happyShift action_99
action_168 (74) = happyShift action_100
action_168 (75) = happyShift action_101
action_168 (76) = happyShift action_102
action_168 (77) = happyShift action_103
action_168 (78) = happyShift action_104
action_168 (79) = happyShift action_53
action_168 (80) = happyShift action_54
action_168 (19) = happyGoto action_162
action_168 (20) = happyGoto action_92
action_168 (21) = happyGoto action_93
action_168 (31) = happyGoto action_199
action_168 (32) = happyGoto action_94
action_168 (34) = happyGoto action_95
action_168 _ = happyFail

action_169 (48) = happyShift action_198
action_169 _ = happyFail

action_170 (44) = happyShift action_197
action_170 _ = happyFail

action_171 _ = happyReduce_43

action_172 (43) = happyShift action_96
action_172 (56) = happyShift action_97
action_172 (59) = happyShift action_98
action_172 (60) = happyShift action_99
action_172 (74) = happyShift action_100
action_172 (75) = happyShift action_101
action_172 (76) = happyShift action_102
action_172 (77) = happyShift action_103
action_172 (78) = happyShift action_104
action_172 (79) = happyShift action_53
action_172 (80) = happyShift action_54
action_172 (19) = happyGoto action_162
action_172 (20) = happyGoto action_92
action_172 (21) = happyGoto action_93
action_172 (30) = happyGoto action_196
action_172 (31) = happyGoto action_164
action_172 (32) = happyGoto action_94
action_172 (34) = happyGoto action_95
action_172 _ = happyReduce_83

action_173 (43) = happyShift action_96
action_173 (56) = happyShift action_97
action_173 (59) = happyShift action_98
action_173 (60) = happyShift action_99
action_173 (74) = happyShift action_100
action_173 (75) = happyShift action_101
action_173 (76) = happyShift action_102
action_173 (77) = happyShift action_103
action_173 (78) = happyShift action_104
action_173 (79) = happyShift action_53
action_173 (80) = happyShift action_54
action_173 (19) = happyGoto action_162
action_173 (20) = happyGoto action_92
action_173 (21) = happyGoto action_93
action_173 (30) = happyGoto action_195
action_173 (31) = happyGoto action_164
action_173 (32) = happyGoto action_94
action_173 (34) = happyGoto action_95
action_173 _ = happyReduce_83

action_174 _ = happyReduce_51

action_175 (43) = happyShift action_96
action_175 (56) = happyShift action_97
action_175 (59) = happyShift action_98
action_175 (60) = happyShift action_99
action_175 (74) = happyShift action_100
action_175 (75) = happyShift action_101
action_175 (76) = happyShift action_102
action_175 (77) = happyShift action_103
action_175 (78) = happyShift action_104
action_175 (79) = happyShift action_53
action_175 (80) = happyShift action_54
action_175 (19) = happyGoto action_194
action_175 (20) = happyGoto action_92
action_175 (21) = happyGoto action_93
action_175 (32) = happyGoto action_94
action_175 (34) = happyGoto action_95
action_175 _ = happyFail

action_176 (78) = happyShift action_193
action_176 (79) = happyShift action_53
action_176 (80) = happyShift action_54
action_176 (33) = happyGoto action_192
action_176 (34) = happyGoto action_52
action_176 _ = happyFail

action_177 (47) = happyShift action_55
action_177 (78) = happyShift action_191
action_177 _ = happyFail

action_178 (40) = happyShift action_190
action_178 _ = happyFail

action_179 _ = happyReduce_64

action_180 (43) = happyShift action_96
action_180 (56) = happyShift action_97
action_180 (59) = happyShift action_98
action_180 (60) = happyShift action_99
action_180 (74) = happyShift action_100
action_180 (75) = happyShift action_101
action_180 (76) = happyShift action_102
action_180 (77) = happyShift action_103
action_180 (78) = happyShift action_104
action_180 (79) = happyShift action_53
action_180 (80) = happyShift action_54
action_180 (19) = happyGoto action_189
action_180 (20) = happyGoto action_92
action_180 (21) = happyGoto action_93
action_180 (32) = happyGoto action_94
action_180 (34) = happyGoto action_95
action_180 _ = happyFail

action_181 (46) = happyShift action_188
action_181 _ = happyFail

action_182 (40) = happyShift action_186
action_182 (49) = happyShift action_187
action_182 _ = happyFail

action_183 _ = happyReduce_56

action_184 _ = happyReduce_25

action_185 _ = happyReduce_58

action_186 _ = happyReduce_63

action_187 (43) = happyShift action_96
action_187 (56) = happyShift action_97
action_187 (59) = happyShift action_98
action_187 (60) = happyShift action_99
action_187 (74) = happyShift action_100
action_187 (75) = happyShift action_101
action_187 (76) = happyShift action_102
action_187 (77) = happyShift action_103
action_187 (78) = happyShift action_104
action_187 (79) = happyShift action_53
action_187 (80) = happyShift action_54
action_187 (19) = happyGoto action_208
action_187 (20) = happyGoto action_92
action_187 (21) = happyGoto action_93
action_187 (32) = happyGoto action_94
action_187 (34) = happyGoto action_95
action_187 _ = happyFail

action_188 _ = happyReduce_57

action_189 (44) = happyShift action_207
action_189 _ = happyFail

action_190 _ = happyReduce_65

action_191 (49) = happyShift action_206
action_191 _ = happyFail

action_192 (47) = happyShift action_55
action_192 (78) = happyShift action_205
action_192 _ = happyFail

action_193 (49) = happyShift action_204
action_193 _ = happyFail

action_194 (44) = happyShift action_203
action_194 _ = happyFail

action_195 (44) = happyShift action_202
action_195 _ = happyFail

action_196 (44) = happyShift action_201
action_196 _ = happyFail

action_197 _ = happyReduce_40

action_198 _ = happyReduce_41

action_199 _ = happyReduce_86

action_200 _ = happyReduce_23

action_201 _ = happyReduce_54

action_202 _ = happyReduce_52

action_203 (45) = happyShift action_133
action_203 (23) = happyGoto action_214
action_203 _ = happyFail

action_204 (43) = happyShift action_96
action_204 (56) = happyShift action_97
action_204 (59) = happyShift action_98
action_204 (60) = happyShift action_99
action_204 (74) = happyShift action_100
action_204 (75) = happyShift action_101
action_204 (76) = happyShift action_102
action_204 (77) = happyShift action_103
action_204 (78) = happyShift action_104
action_204 (79) = happyShift action_53
action_204 (80) = happyShift action_54
action_204 (19) = happyGoto action_213
action_204 (20) = happyGoto action_92
action_204 (21) = happyGoto action_93
action_204 (32) = happyGoto action_94
action_204 (34) = happyGoto action_95
action_204 _ = happyFail

action_205 (49) = happyShift action_212
action_205 _ = happyFail

action_206 (43) = happyShift action_96
action_206 (56) = happyShift action_97
action_206 (59) = happyShift action_98
action_206 (60) = happyShift action_99
action_206 (74) = happyShift action_100
action_206 (75) = happyShift action_101
action_206 (76) = happyShift action_102
action_206 (77) = happyShift action_103
action_206 (78) = happyShift action_104
action_206 (79) = happyShift action_53
action_206 (80) = happyShift action_54
action_206 (19) = happyGoto action_211
action_206 (20) = happyGoto action_92
action_206 (21) = happyGoto action_93
action_206 (32) = happyGoto action_94
action_206 (34) = happyGoto action_95
action_206 _ = happyFail

action_207 (45) = happyShift action_133
action_207 (23) = happyGoto action_210
action_207 _ = happyFail

action_208 (40) = happyShift action_209
action_208 _ = happyFail

action_209 _ = happyReduce_62

action_210 (58) = happyShift action_218
action_210 _ = happyReduce_59

action_211 (40) = happyShift action_217
action_211 _ = happyFail

action_212 (43) = happyShift action_96
action_212 (56) = happyShift action_97
action_212 (59) = happyShift action_98
action_212 (60) = happyShift action_99
action_212 (74) = happyShift action_100
action_212 (75) = happyShift action_101
action_212 (76) = happyShift action_102
action_212 (77) = happyShift action_103
action_212 (78) = happyShift action_104
action_212 (79) = happyShift action_53
action_212 (80) = happyShift action_54
action_212 (19) = happyGoto action_216
action_212 (20) = happyGoto action_92
action_212 (21) = happyGoto action_93
action_212 (32) = happyGoto action_94
action_212 (34) = happyGoto action_95
action_212 _ = happyFail

action_213 (40) = happyShift action_215
action_213 _ = happyFail

action_214 _ = happyReduce_66

action_215 (43) = happyShift action_96
action_215 (56) = happyShift action_97
action_215 (59) = happyShift action_98
action_215 (60) = happyShift action_99
action_215 (74) = happyShift action_100
action_215 (75) = happyShift action_101
action_215 (76) = happyShift action_102
action_215 (77) = happyShift action_103
action_215 (78) = happyShift action_104
action_215 (79) = happyShift action_53
action_215 (80) = happyShift action_54
action_215 (19) = happyGoto action_221
action_215 (20) = happyGoto action_92
action_215 (21) = happyGoto action_93
action_215 (32) = happyGoto action_94
action_215 (34) = happyGoto action_95
action_215 _ = happyFail

action_216 (40) = happyShift action_220
action_216 _ = happyFail

action_217 _ = happyReduce_61

action_218 (40) = happyShift action_132
action_218 (43) = happyShift action_96
action_218 (45) = happyShift action_133
action_218 (56) = happyShift action_97
action_218 (57) = happyShift action_134
action_218 (59) = happyShift action_98
action_218 (60) = happyShift action_99
action_218 (61) = happyShift action_135
action_218 (71) = happyShift action_136
action_218 (72) = happyShift action_137
action_218 (73) = happyShift action_138
action_218 (74) = happyShift action_100
action_218 (75) = happyShift action_101
action_218 (76) = happyShift action_102
action_218 (77) = happyShift action_103
action_218 (78) = happyShift action_104
action_218 (79) = happyShift action_53
action_218 (80) = happyShift action_54
action_218 (19) = happyGoto action_127
action_218 (20) = happyGoto action_92
action_218 (21) = happyGoto action_93
action_218 (23) = happyGoto action_129
action_218 (24) = happyGoto action_219
action_218 (32) = happyGoto action_94
action_218 (33) = happyGoto action_131
action_218 (34) = happyGoto action_111
action_218 _ = happyFail

action_219 _ = happyReduce_60

action_220 (43) = happyShift action_96
action_220 (56) = happyShift action_97
action_220 (59) = happyShift action_98
action_220 (60) = happyShift action_99
action_220 (74) = happyShift action_100
action_220 (75) = happyShift action_101
action_220 (76) = happyShift action_102
action_220 (77) = happyShift action_103
action_220 (78) = happyShift action_104
action_220 (79) = happyShift action_53
action_220 (80) = happyShift action_54
action_220 (19) = happyGoto action_223
action_220 (20) = happyGoto action_92
action_220 (21) = happyGoto action_93
action_220 (32) = happyGoto action_94
action_220 (34) = happyGoto action_95
action_220 _ = happyFail

action_221 (40) = happyShift action_222
action_221 _ = happyFail

action_222 (43) = happyShift action_96
action_222 (56) = happyShift action_97
action_222 (59) = happyShift action_98
action_222 (60) = happyShift action_99
action_222 (74) = happyShift action_100
action_222 (75) = happyShift action_101
action_222 (76) = happyShift action_102
action_222 (77) = happyShift action_103
action_222 (78) = happyShift action_104
action_222 (79) = happyShift action_53
action_222 (80) = happyShift action_54
action_222 (19) = happyGoto action_225
action_222 (20) = happyGoto action_92
action_222 (21) = happyGoto action_93
action_222 (32) = happyGoto action_94
action_222 (34) = happyGoto action_95
action_222 _ = happyFail

action_223 (40) = happyShift action_224
action_223 _ = happyFail

action_224 (43) = happyShift action_96
action_224 (56) = happyShift action_97
action_224 (59) = happyShift action_98
action_224 (60) = happyShift action_99
action_224 (74) = happyShift action_100
action_224 (75) = happyShift action_101
action_224 (76) = happyShift action_102
action_224 (77) = happyShift action_103
action_224 (78) = happyShift action_104
action_224 (79) = happyShift action_53
action_224 (80) = happyShift action_54
action_224 (19) = happyGoto action_227
action_224 (20) = happyGoto action_92
action_224 (21) = happyGoto action_93
action_224 (32) = happyGoto action_94
action_224 (34) = happyGoto action_95
action_224 _ = happyFail

action_225 (44) = happyShift action_226
action_225 _ = happyFail

action_226 (45) = happyShift action_133
action_226 (23) = happyGoto action_229
action_226 _ = happyFail

action_227 (44) = happyShift action_228
action_227 _ = happyFail

action_228 (45) = happyShift action_133
action_228 (23) = happyGoto action_230
action_228 _ = happyFail

action_229 _ = happyReduce_67

action_230 _ = happyReduce_68

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn25  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 (happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn5
		 (
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	_
	_
	 =  HappyAbsSyn6
		 (
	)

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 (
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (
	)

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 _
	_
	_
	 =  HappyAbsSyn9
		 (
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn10
		 (
	)

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn10
		 (
	)

happyReduce_13 = happyReduce 10 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	(HappyTerminal (TokenClassType happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn38  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (let attr = if happy_var_3 then ["static"] else []
            in  Class attr happy_var_5 happy_var_6 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 7 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyTerminal (TokenClassType happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Interface happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_0  12 happyReduction_15
happyReduction_15  =  HappyAbsSyn12
		 (Nothing
	)

happyReduce_16 = happySpecReduce_2  12 happyReduction_16
happyReduction_16 (HappyTerminal (TokenClassType happy_var_2))
	_
	 =  HappyAbsSyn12
		 (Just happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  13 happyReduction_17
happyReduction_17  =  HappyAbsSyn13
		 ([]
	)

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	(HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn15
		 (\attr -> FieldDecl attr happy_var_1 happy_var_2 Nothing
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 15 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (\attr -> FieldDecl attr happy_var_1 happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (\attr -> MethodDecl attr happy_var_1 happy_var_2 happy_var_4 (toStmt happy_var_7)
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 17 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (AbstractMethodDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 7 18 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenClassType happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (ConstrDecl happy_var_1 happy_var_3 (toStmt happy_var_6)
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 19 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Prim happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (Prim "==" [happy_var_1, happy_var_3]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  19 happyReduction_28
happyReduction_28 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (Prim "+" [happy_var_1, happy_var_3]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  19 happyReduction_29
happyReduction_29 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (Prim "-" [happy_var_1, happy_var_3]
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (Prim "*" [happy_var_1, happy_var_3]
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  19 happyReduction_31
happyReduction_31 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn19
		 (Prim "/" [happy_var_1, happy_var_3]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  20 happyReduction_33
happyReduction_33 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn20
		 (ConstTrue
	)

happyReduce_35 = happySpecReduce_1  20 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn20
		 (ConstFalse
	)

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn20
		 (ConstNull
	)

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn20
		 (ConstNum happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyTerminal (TokenLit happy_var_1))
	 =  HappyAbsSyn20
		 (ConstLit happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn20
		 (ConstChar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 20 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (New (TypeName happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 5 20 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (New (ArrayTypeName (TypeName happy_var_2)) happy_var_4
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_3  20 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 20 happyReduction_43
happyReduction_43 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Cast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  20 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "<" [happy_var_1, happy_var_3]
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "!=" [happy_var_1, happy_var_3]
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  20 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "++" [happy_var_1, happy_var_1]
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  20 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "--" [happy_var_1, happy_var_1]
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  21 happyReduction_49
happyReduction_49 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn21
		 (Var happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  21 happyReduction_50
happyReduction_50 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Field happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happyReduce 4 21 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Prim "[]" [happy_var_1, happy_var_3]
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 6 21 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Invoke happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  21 happyReduction_53
happyReduction_53 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn21
		 (StaticField (TypeName happy_var_1) happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happyReduce 6 21 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (StaticInvoke (TypeName happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_0  22 happyReduction_55
happyReduction_55  =  HappyAbsSyn22
		 ([]
	)

happyReduce_56 = happySpecReduce_2  22 happyReduction_56
happyReduction_56 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  23 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (Block $ toStmt happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  24 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn24
		 (Expr happy_var_1
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happyReduce 5 24 happyReduction_59
happyReduction_59 ((HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Ite happy_var_3 happy_var_5 NoStmt
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 7 24 happyReduction_60
happyReduction_60 ((HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Ite happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_61 = happyReduce 6 24 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (LocalVarDecl happy_var_2 happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 5 24 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (LocalVarDecl happy_var_1 happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_3  24 happyReduction_63
happyReduction_63 _
	(HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn24
		 (LocalVarDecl happy_var_1 happy_var_2 Nothing
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  24 happyReduction_64
happyReduction_64 _
	_
	 =  HappyAbsSyn24
		 (Return Nothing
	)

happyReduce_65 = happySpecReduce_3  24 happyReduction_65
happyReduction_65 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Return (Just happy_var_2)
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happyReduce 5 24 happyReduction_66
happyReduction_66 ((HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_67 = happyReduce 11 24 happyReduction_67
happyReduction_67 ((HappyAbsSyn23  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (For Nothing happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_68 = happyReduce 12 24 happyReduction_68
happyReduction_68 ((HappyAbsSyn23  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_4)) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (For (Just happy_var_3) happy_var_4 happy_var_6 happy_var_8 happy_var_10 happy_var_12
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_1  24 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn24
		 (NoStmt
	)

happyReduce_70 = happySpecReduce_1  24 happyReduction_70
happyReduction_70 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  25 happyReduction_71
happyReduction_71 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  25 happyReduction_72
happyReduction_72 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_0  26 happyReduction_73
happyReduction_73  =  HappyAbsSyn26
		 ([]
	)

happyReduce_74 = happyReduce 5 26 happyReduction_74
happyReduction_74 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn38  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (let attr = if happy_var_3 then ["static"] else [] in
            (happy_var_4 attr) : happy_var_5
	) `HappyStk` happyRest

happyReduce_75 = happyReduce 5 26 happyReduction_75
happyReduction_75 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn38  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (let attr = if happy_var_3 then ["static"] else [] in
            (happy_var_4 attr) : happy_var_5
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 5 26 happyReduction_76
happyReduction_76 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (happy_var_4 : happy_var_5
	) `HappyStk` happyRest

happyReduce_77 = happySpecReduce_0  27 happyReduction_77
happyReduction_77  =  HappyAbsSyn27
		 ([]
	)

happyReduce_78 = happyReduce 4 27 happyReduction_78
happyReduction_78 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (happy_var_3 : happy_var_4
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_0  28 happyReduction_79
happyReduction_79  =  HappyAbsSyn28
		 ([]
	)

happyReduce_80 = happySpecReduce_1  28 happyReduction_80
happyReduction_80 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  29 happyReduction_81
happyReduction_81 (HappyTerminal (TokenVar happy_var_3))
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn29
		 ([(happy_var_2, happy_var_3)]
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happyReduce 5 29 happyReduction_82
happyReduction_82 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 ((happy_var_2, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_83 = happySpecReduce_0  30 happyReduction_83
happyReduction_83  =  HappyAbsSyn30
		 ([]
	)

happyReduce_84 = happySpecReduce_1  30 happyReduction_84
happyReduction_84 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  31 happyReduction_85
happyReduction_85 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  31 happyReduction_86
happyReduction_86 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  32 happyReduction_87
happyReduction_87 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  32 happyReduction_88
happyReduction_88 (HappyTerminal (TokenLit happy_var_1))
	 =  HappyAbsSyn32
		 (ConstLit happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  32 happyReduction_89
happyReduction_89 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn32
		 (ConstChar happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  32 happyReduction_90
happyReduction_90 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn32
		 (ConstNum happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  32 happyReduction_91
happyReduction_91 _
	 =  HappyAbsSyn32
		 (ConstTrue
	)

happyReduce_92 = happySpecReduce_1  32 happyReduction_92
happyReduction_92 _
	 =  HappyAbsSyn32
		 (ConstFalse
	)

happyReduce_93 = happySpecReduce_1  32 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn32
		 (ConstNull
	)

happyReduce_94 = happySpecReduce_1  33 happyReduction_94
happyReduction_94 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (TypeName happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  33 happyReduction_95
happyReduction_95 _
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (ArrayTypeName happy_var_1
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  34 happyReduction_96
happyReduction_96 (HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  34 happyReduction_97
happyReduction_97 (HappyTerminal (TokenPrimitiveType happy_var_1))
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_0  35 happyReduction_98
happyReduction_98  =  HappyAbsSyn35
		 (False
	)

happyReduce_99 = happySpecReduce_1  35 happyReduction_99
happyReduction_99 _
	 =  HappyAbsSyn35
		 (True
	)

happyReduce_100 = happySpecReduce_1  36 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn36
		 (1
	)

happyReduce_101 = happySpecReduce_1  36 happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn36
		 (2
	)

happyReduce_102 = happySpecReduce_1  36 happyReduction_102
happyReduction_102 _
	 =  HappyAbsSyn36
		 (3
	)

happyReduce_103 = happySpecReduce_0  37 happyReduction_103
happyReduction_103  =  HappyAbsSyn37
		 (False
	)

happyReduce_104 = happySpecReduce_1  37 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn37
		 (True
	)

happyReduce_105 = happySpecReduce_0  38 happyReduction_105
happyReduction_105  =  HappyAbsSyn38
		 (False
	)

happyReduce_106 = happySpecReduce_1  38 happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn38
		 (True
	)

happyReduce_107 = happySpecReduce_0  39 happyReduction_107
happyReduction_107  =  HappyAbsSyn39
		 (False
	)

happyReduce_108 = happySpecReduce_1  39 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn39
		 (True
	)

happyNewToken action sts stk [] =
	action 86 86 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSemiColon -> cont 40;
	TokenDot -> cont 41;
	TokenComma -> cont 42;
	TokenOP -> cont 43;
	TokenCP -> cont 44;
	TokenOB -> cont 45;
	TokenCB -> cont 46;
	TokenOBK -> cont 47;
	TokenCBK -> cont 48;
	TokenEQ -> cont 49;
	TokenPlus -> cont 50;
	TokenMinus -> cont 51;
	TokenStar -> cont 52;
	TokenDiv -> cont 53;
	TokenClass -> cont 54;
	TokenExtends -> cont 55;
	TokenNew -> cont 56;
	TokenIf -> cont 57;
	TokenElse -> cont 58;
	TokenTrue -> cont 59;
	TokenFalse -> cont 60;
	TokenReturn -> cont 61;
	TokenPackage -> cont 62;
	TokenImport -> cont 63;
	TokenPublic -> cont 64;
	TokenProtected -> cont 65;
	TokenPrivate -> cont 66;
	TokenStatic -> cont 67;
	TokenAbstract -> cont 68;
	TokenInterface -> cont 69;
	TokenImplements -> cont 70;
	TokenFinal -> cont 71;
	TokenFor -> cont 72;
	TokenWhile -> cont 73;
	TokenNull -> cont 74;
	TokenNum happy_dollar_dollar -> cont 75;
	TokenLit happy_dollar_dollar -> cont 76;
	TokenChar happy_dollar_dollar -> cont 77;
	TokenVar happy_dollar_dollar -> cont 78;
	TokenClassType happy_dollar_dollar -> cont 79;
	TokenPrimitiveType happy_dollar_dollar -> cont 80;
	TokenEqual -> cont 81;
	TokenLT -> cont 82;
	TokenNotEq -> cont 83;
	TokenInc -> cont 84;
	TokenDec -> cont 85;
	_ -> happyError' (tk:tks)
	}

happyError_ 86 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseprog tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError toks = error ("Parse error at " ++
			 (concat 
			  $ intersperse " " 
			  $ map toStr 
			  $ take 50
			  $ toks))

toStr :: Token -> String
toStr TokenSemiColon = ";"
toStr TokenDot       = "."
toStr TokenComma     = ","
toStr TokenOP        = "("
toStr TokenCP        = ")"
toStr TokenOB        = "{"
toStr TokenCB        = "}"
toStr TokenOBK       = "["
toStr TokenCBK       = "]"
toStr TokenEQ        = "="
toStr TokenPlus      = "+"
toStr TokenMinus     = "-"
toStr TokenStar      = "*"
toStr TokenDiv       = "/"
toStr TokenClass     = "class"
toStr TokenExtends   = "extends"
toStr TokenNew       = "new"
toStr TokenIf        = "if"
toStr TokenElse      = "else"
toStr TokenTrue      = "true"
toStr TokenFalse     = "false"
toStr TokenNull      = "null"
toStr TokenReturn    = "return"

toStr TokenPackage   = "package"
toStr TokenImport    = "import"
toStr TokenPublic    = "public"
toStr TokenProtected = "protected"
toStr TokenPrivate   = "private"
toStr TokenStatic    = "static"
toStr TokenAbstract  = "abstract"
toStr TokenInterface = "interface"
toStr TokenImplements = "implements"
toStr TokenFinal = "final"
toStr TokenFor = "for"
toStr TokenWhile = "while"

toStr TokenEqual     = "=="
toStr TokenInc       = "++"
toStr TokenDec       = "--"
toStr TokenLT        = "<"
toStr TokenNotEq     = "!="
toStr (TokenNum n)   = show n
toStr (TokenLit s)   = show s
toStr (TokenChar c)  = show c
toStr (TokenVar v)   = v
toStr (TokenClassType t)  = t
toStr (TokenPrimitiveType t)  = t

data Token = TokenSemiColon
     	   | TokenDot
     	   | TokenComma
	   | TokenOP
	   | TokenCP
	   | TokenOB
	   | TokenCB
	   | TokenOBK
	   | TokenCBK
	   | TokenEQ
	   | TokenPlus
	   | TokenMinus
	   | TokenStar
	   | TokenDiv
	   | TokenClass
	   | TokenExtends
	   | TokenNew
	   | TokenIf
	   | TokenElse
	   | TokenTrue
	   | TokenFalse
	   | TokenReturn
	   | TokenNull

	   | TokenPackage
	   | TokenImport
	   | TokenPublic
	   | TokenProtected
	   | TokenPrivate
	   | TokenStatic
	   | TokenAbstract
	   | TokenInterface
	   | TokenImplements
	   | TokenFinal
	   | TokenFor
	   | TokenWhile

	   | TokenNum String
	   | TokenLit String
	   | TokenChar String
	   | TokenVar String
	   | TokenClassType String
	   | TokenPrimitiveType String
	   | TokenEqual
           | TokenLT
           | TokenNotEq
           | TokenInc
           | TokenDec
	   deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer ('/':'/':cs) = lexer (dropWhile (/= '\n') cs)
lexer ('/':'*':cs) = lexer (dropComment cs)
lexer (';':cs) = TokenSemiColon : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('{':cs) = TokenOB : lexer cs
lexer ('}':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenOBK : lexer cs
lexer (']':cs) = TokenCBK : lexer cs
lexer ('=':'=':cs) = TokenEqual : lexer cs
lexer ('!':'=':cs) = TokenNotEq : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('+':'+':cs) = TokenInc : lexer cs
lexer ('-':'-':cs) = TokenDec : lexer cs
lexer ('=':cs) = TokenEQ : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenStar : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer (c:cs) | isSpace c = lexer cs
      	     | isAlpha c = lexVar (c:cs)
	     | isDigit c = lexNum (c:cs)
	     | c == '\"' = lexLit (c:cs)
	     | c == '\'' = lexChar (c:cs)
lexer (c:cs) = error [c]

dropComment [] = []
dropComment [c] = []
dropComment ('*':'/':cs) = cs
dropComment (c:cs) = dropComment cs

isVarChar c = isAlpha c || isDigit c || c == '_' || c == '\''

lexVar cs = case span isVarChar cs of
       	       ("class", rest)   -> TokenClass : lexer rest
       	       ("extends", rest) -> TokenExtends : lexer rest
       	       ("new", rest)     -> TokenNew : lexer rest
       	       ("if", rest)      -> TokenIf : lexer rest
       	       ("else", rest)    -> TokenElse : lexer rest
       	       ("true", rest)    -> TokenTrue : lexer rest
       	       ("false", rest)   -> TokenFalse : lexer rest
       	       ("return", rest)  -> TokenReturn : lexer rest
       	       ("null", rest)    -> TokenNull : lexer rest
       	       ("package", rest) -> TokenPackage : lexer rest
       	       ("import", rest)  -> TokenImport : lexer rest
       	       ("public", rest)  -> TokenPublic : lexer rest
       	       ("protected", rest)  -> TokenProtected : lexer rest
       	       ("private", rest)  -> TokenPrivate : lexer rest
       	       ("static", rest)  -> TokenStatic : lexer rest
       	       ("abstract", rest) -> TokenAbstract : lexer rest
       	       ("interface", rest) -> TokenInterface : lexer rest
       	       ("implements", rest) -> TokenImplements : lexer rest
       	       ("final", rest)   -> TokenFinal : lexer rest
       	       ("for", rest)     -> TokenFor : lexer rest
       	       ("while", rest)   -> TokenWhile : lexer rest
	       (ident, rest)     -> varOrType ident : lexer rest

varOrType s = if cond1 then TokenClassType s 
              else if cond2 then TokenPrimitiveType s 
              else TokenVar s
  where
     cond1 = elem (head s) ['A'..'Z']
     cond2 = elem s ["int", "boolean", "char", "float", "byte", "void"]

lexNum cs = TokenNum num : lexer rest
   where (num,rest) = span isDigit cs

lexLit (_:cs) = TokenLit lit : lexer cs'
   where
      (lit,cs') = tw "" cs
      tw a [] = error ("Missing \" in " ++ reverse a)
      tw a (c:cs) | c /= '\"' = tw (c:a) cs
                  | otherwise = (reverse a, cs)

lexChar (_:cs) = TokenChar charstr : lexer cs'
   where
      (charstr,cs') = tw "" cs
      tw a [] = error ("Missing \' in " ++ reverse a)
      tw a (c:cs) | c /= '\'' = tw (c:a) cs
                  | otherwise = (reverse a, cs)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

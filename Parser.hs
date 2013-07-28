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

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40
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
	| HappyAbsSyn40 t40

action_0 (63) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (63) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (64) = happyShift action_12
action_2 (8) = happyGoto action_9
action_2 (9) = happyGoto action_10
action_2 (10) = happyGoto action_11
action_2 _ = happyReduce_8

action_3 _ = happyReduce_3

action_4 (79) = happyShift action_7
action_4 (80) = happyShift action_8
action_4 (7) = happyGoto action_6
action_4 _ = happyFail

action_5 (87) = happyAccept
action_5 _ = happyFail

action_6 (41) = happyShift action_23
action_6 _ = happyFail

action_7 (42) = happyShift action_22
action_7 _ = happyReduce_5

action_8 _ = happyReduce_6

action_9 (65) = happyShift action_19
action_9 (66) = happyShift action_20
action_9 (67) = happyShift action_21
action_9 (11) = happyGoto action_15
action_9 (26) = happyGoto action_16
action_9 (36) = happyGoto action_17
action_9 (37) = happyGoto action_18
action_9 _ = happyReduce_100

action_10 (64) = happyShift action_12
action_10 (9) = happyGoto action_10
action_10 (10) = happyGoto action_14
action_10 _ = happyReduce_11

action_11 _ = happyReduce_9

action_12 (79) = happyShift action_7
action_12 (80) = happyShift action_8
action_12 (7) = happyGoto action_13
action_12 _ = happyFail

action_13 (41) = happyShift action_29
action_13 _ = happyFail

action_14 _ = happyReduce_12

action_15 (65) = happyShift action_19
action_15 (66) = happyShift action_20
action_15 (67) = happyShift action_21
action_15 (87) = happyReduce_73
action_15 (11) = happyGoto action_15
action_15 (26) = happyGoto action_28
action_15 (36) = happyGoto action_17
action_15 (37) = happyGoto action_18
action_15 _ = happyReduce_100

action_16 _ = happyReduce_1

action_17 (69) = happyShift action_26
action_17 (70) = happyShift action_27
action_17 (38) = happyGoto action_25
action_17 _ = happyReduce_105

action_18 _ = happyReduce_101

action_19 _ = happyReduce_102

action_20 _ = happyReduce_103

action_21 _ = happyReduce_104

action_22 (79) = happyShift action_7
action_22 (80) = happyShift action_8
action_22 (7) = happyGoto action_24
action_22 _ = happyFail

action_23 _ = happyReduce_4

action_24 _ = happyReduce_7

action_25 (68) = happyShift action_32
action_25 (39) = happyGoto action_31
action_25 _ = happyReduce_107

action_26 _ = happyReduce_106

action_27 (80) = happyShift action_30
action_27 _ = happyFail

action_28 _ = happyReduce_74

action_29 _ = happyReduce_10

action_30 (56) = happyShift action_35
action_30 (13) = happyGoto action_34
action_30 _ = happyReduce_17

action_31 (55) = happyShift action_33
action_31 _ = happyFail

action_32 _ = happyReduce_108

action_33 (80) = happyShift action_39
action_33 _ = happyFail

action_34 (46) = happyShift action_38
action_34 _ = happyFail

action_35 (80) = happyShift action_37
action_35 (15) = happyGoto action_36
action_35 _ = happyFail

action_36 _ = happyReduce_18

action_37 (43) = happyShift action_44
action_37 _ = happyReduce_21

action_38 (65) = happyShift action_19
action_38 (66) = happyShift action_20
action_38 (67) = happyShift action_21
action_38 (72) = happyReduce_100
action_38 (80) = happyReduce_100
action_38 (81) = happyReduce_100
action_38 (28) = happyGoto action_42
action_38 (36) = happyGoto action_43
action_38 (37) = happyGoto action_18
action_38 _ = happyReduce_79

action_39 (56) = happyShift action_41
action_39 (12) = happyGoto action_40
action_39 _ = happyReduce_15

action_40 (71) = happyShift action_51
action_40 (14) = happyGoto action_50
action_40 _ = happyReduce_19

action_41 (80) = happyShift action_49
action_41 _ = happyFail

action_42 (47) = happyShift action_48
action_42 _ = happyFail

action_43 (72) = happyShift action_47
action_43 (40) = happyGoto action_46
action_43 _ = happyReduce_109

action_44 (80) = happyShift action_37
action_44 (15) = happyGoto action_45
action_44 _ = happyFail

action_45 _ = happyReduce_22

action_46 (80) = happyShift action_57
action_46 (81) = happyShift action_58
action_46 (18) = happyGoto action_54
action_46 (34) = happyGoto action_55
action_46 (35) = happyGoto action_56
action_46 _ = happyFail

action_47 _ = happyReduce_110

action_48 _ = happyReduce_14

action_49 _ = happyReduce_16

action_50 (46) = happyShift action_53
action_50 _ = happyFail

action_51 (80) = happyShift action_37
action_51 (15) = happyGoto action_52
action_51 _ = happyFail

action_52 _ = happyReduce_20

action_53 (65) = happyShift action_19
action_53 (66) = happyShift action_20
action_53 (67) = happyShift action_21
action_53 (68) = happyReduce_100
action_53 (72) = happyReduce_100
action_53 (80) = happyReduce_100
action_53 (81) = happyReduce_100
action_53 (27) = happyGoto action_62
action_53 (36) = happyGoto action_63
action_53 (37) = happyGoto action_18
action_53 _ = happyReduce_75

action_54 (65) = happyShift action_19
action_54 (66) = happyShift action_20
action_54 (67) = happyShift action_21
action_54 (72) = happyReduce_100
action_54 (80) = happyReduce_100
action_54 (81) = happyReduce_100
action_54 (28) = happyGoto action_61
action_54 (36) = happyGoto action_43
action_54 (37) = happyGoto action_18
action_54 _ = happyReduce_79

action_55 (48) = happyShift action_59
action_55 (79) = happyShift action_60
action_55 _ = happyFail

action_56 _ = happyReduce_96

action_57 _ = happyReduce_98

action_58 _ = happyReduce_99

action_59 (49) = happyShift action_67
action_59 _ = happyFail

action_60 (44) = happyShift action_66
action_60 _ = happyFail

action_61 _ = happyReduce_80

action_62 (47) = happyShift action_65
action_62 _ = happyFail

action_63 (72) = happyShift action_47
action_63 (40) = happyGoto action_64
action_63 _ = happyReduce_109

action_64 (68) = happyShift action_32
action_64 (39) = happyGoto action_71
action_64 _ = happyReduce_107

action_65 _ = happyReduce_13

action_66 (72) = happyShift action_47
action_66 (80) = happyReduce_109
action_66 (81) = happyReduce_109
action_66 (29) = happyGoto action_68
action_66 (30) = happyGoto action_69
action_66 (40) = happyGoto action_70
action_66 _ = happyReduce_81

action_67 _ = happyReduce_97

action_68 (45) = happyShift action_78
action_68 _ = happyFail

action_69 _ = happyReduce_82

action_70 (80) = happyShift action_57
action_70 (81) = happyShift action_58
action_70 (34) = happyGoto action_77
action_70 (35) = happyGoto action_56
action_70 _ = happyFail

action_71 (80) = happyShift action_76
action_71 (81) = happyShift action_58
action_71 (16) = happyGoto action_72
action_71 (17) = happyGoto action_73
action_71 (19) = happyGoto action_74
action_71 (34) = happyGoto action_75
action_71 (35) = happyGoto action_56
action_71 _ = happyFail

action_72 (65) = happyShift action_19
action_72 (66) = happyShift action_20
action_72 (67) = happyShift action_21
action_72 (68) = happyReduce_100
action_72 (72) = happyReduce_100
action_72 (80) = happyReduce_100
action_72 (81) = happyReduce_100
action_72 (27) = happyGoto action_85
action_72 (36) = happyGoto action_63
action_72 (37) = happyGoto action_18
action_72 _ = happyReduce_75

action_73 (65) = happyShift action_19
action_73 (66) = happyShift action_20
action_73 (67) = happyShift action_21
action_73 (68) = happyReduce_100
action_73 (72) = happyReduce_100
action_73 (80) = happyReduce_100
action_73 (81) = happyReduce_100
action_73 (27) = happyGoto action_84
action_73 (36) = happyGoto action_63
action_73 (37) = happyGoto action_18
action_73 _ = happyReduce_75

action_74 (65) = happyShift action_19
action_74 (66) = happyShift action_20
action_74 (67) = happyShift action_21
action_74 (68) = happyReduce_100
action_74 (72) = happyReduce_100
action_74 (80) = happyReduce_100
action_74 (81) = happyReduce_100
action_74 (27) = happyGoto action_83
action_74 (36) = happyGoto action_63
action_74 (37) = happyGoto action_18
action_74 _ = happyReduce_75

action_75 (48) = happyShift action_59
action_75 (79) = happyShift action_82
action_75 _ = happyFail

action_76 (44) = happyShift action_81
action_76 _ = happyReduce_98

action_77 (48) = happyShift action_59
action_77 (79) = happyShift action_80
action_77 _ = happyFail

action_78 (41) = happyShift action_79
action_78 _ = happyFail

action_79 _ = happyReduce_26

action_80 (43) = happyShift action_90
action_80 _ = happyReduce_83

action_81 (72) = happyShift action_47
action_81 (80) = happyReduce_109
action_81 (81) = happyReduce_109
action_81 (29) = happyGoto action_89
action_81 (30) = happyGoto action_69
action_81 (40) = happyGoto action_70
action_81 _ = happyReduce_81

action_82 (41) = happyShift action_86
action_82 (44) = happyShift action_87
action_82 (50) = happyShift action_88
action_82 _ = happyFail

action_83 _ = happyReduce_78

action_84 _ = happyReduce_77

action_85 _ = happyReduce_76

action_86 _ = happyReduce_23

action_87 (72) = happyShift action_47
action_87 (80) = happyReduce_109
action_87 (81) = happyReduce_109
action_87 (29) = happyGoto action_107
action_87 (30) = happyGoto action_69
action_87 (40) = happyGoto action_70
action_87 _ = happyReduce_81

action_88 (44) = happyShift action_98
action_88 (57) = happyShift action_99
action_88 (60) = happyShift action_100
action_88 (61) = happyShift action_101
action_88 (75) = happyShift action_102
action_88 (76) = happyShift action_103
action_88 (77) = happyShift action_104
action_88 (78) = happyShift action_105
action_88 (79) = happyShift action_106
action_88 (80) = happyShift action_57
action_88 (81) = happyShift action_58
action_88 (20) = happyGoto action_93
action_88 (21) = happyGoto action_94
action_88 (22) = happyGoto action_95
action_88 (33) = happyGoto action_96
action_88 (35) = happyGoto action_97
action_88 _ = happyFail

action_89 (45) = happyShift action_92
action_89 _ = happyFail

action_90 (72) = happyShift action_47
action_90 (30) = happyGoto action_91
action_90 (40) = happyGoto action_70
action_90 _ = happyReduce_109

action_91 _ = happyReduce_84

action_92 (46) = happyShift action_128
action_92 _ = happyFail

action_93 (41) = happyShift action_127
action_93 _ = happyFail

action_94 _ = happyReduce_34

action_95 (42) = happyShift action_124
action_95 (48) = happyShift action_125
action_95 (50) = happyShift action_126
action_95 (51) = happyReduce_89
action_95 (52) = happyReduce_89
action_95 (53) = happyReduce_89
action_95 (54) = happyReduce_89
action_95 (82) = happyReduce_89
action_95 (83) = happyReduce_89
action_95 (84) = happyReduce_89
action_95 (85) = happyReduce_89
action_95 (86) = happyReduce_89
action_95 _ = happyReduce_35

action_96 (51) = happyShift action_115
action_96 (52) = happyShift action_116
action_96 (53) = happyShift action_117
action_96 (54) = happyShift action_118
action_96 (82) = happyShift action_119
action_96 (83) = happyShift action_120
action_96 (84) = happyShift action_121
action_96 (85) = happyShift action_122
action_96 (86) = happyShift action_123
action_96 _ = happyFail

action_97 (42) = happyShift action_114
action_97 _ = happyFail

action_98 (44) = happyShift action_98
action_98 (57) = happyShift action_99
action_98 (60) = happyShift action_100
action_98 (61) = happyShift action_101
action_98 (75) = happyShift action_102
action_98 (76) = happyShift action_103
action_98 (77) = happyShift action_104
action_98 (78) = happyShift action_105
action_98 (79) = happyShift action_106
action_98 (80) = happyShift action_57
action_98 (81) = happyShift action_58
action_98 (20) = happyGoto action_111
action_98 (21) = happyGoto action_94
action_98 (22) = happyGoto action_95
action_98 (33) = happyGoto action_96
action_98 (34) = happyGoto action_112
action_98 (35) = happyGoto action_113
action_98 _ = happyFail

action_99 (80) = happyShift action_57
action_99 (81) = happyShift action_58
action_99 (35) = happyGoto action_110
action_99 _ = happyFail

action_100 (51) = happyReduce_93
action_100 (52) = happyReduce_93
action_100 (53) = happyReduce_93
action_100 (54) = happyReduce_93
action_100 (82) = happyReduce_93
action_100 (83) = happyReduce_93
action_100 (84) = happyReduce_93
action_100 (85) = happyReduce_93
action_100 (86) = happyReduce_93
action_100 _ = happyReduce_36

action_101 (51) = happyReduce_94
action_101 (52) = happyReduce_94
action_101 (53) = happyReduce_94
action_101 (54) = happyReduce_94
action_101 (82) = happyReduce_94
action_101 (83) = happyReduce_94
action_101 (84) = happyReduce_94
action_101 (85) = happyReduce_94
action_101 (86) = happyReduce_94
action_101 _ = happyReduce_37

action_102 (51) = happyReduce_95
action_102 (52) = happyReduce_95
action_102 (53) = happyReduce_95
action_102 (54) = happyReduce_95
action_102 (82) = happyReduce_95
action_102 (83) = happyReduce_95
action_102 (84) = happyReduce_95
action_102 (85) = happyReduce_95
action_102 (86) = happyReduce_95
action_102 _ = happyReduce_38

action_103 (51) = happyReduce_92
action_103 (52) = happyReduce_92
action_103 (53) = happyReduce_92
action_103 (54) = happyReduce_92
action_103 (82) = happyReduce_92
action_103 (83) = happyReduce_92
action_103 (84) = happyReduce_92
action_103 (85) = happyReduce_92
action_103 (86) = happyReduce_92
action_103 _ = happyReduce_39

action_104 (51) = happyReduce_90
action_104 (52) = happyReduce_90
action_104 (53) = happyReduce_90
action_104 (54) = happyReduce_90
action_104 (82) = happyReduce_90
action_104 (83) = happyReduce_90
action_104 (84) = happyReduce_90
action_104 (85) = happyReduce_90
action_104 (86) = happyReduce_90
action_104 _ = happyReduce_40

action_105 (51) = happyReduce_91
action_105 (52) = happyReduce_91
action_105 (53) = happyReduce_91
action_105 (54) = happyReduce_91
action_105 (82) = happyReduce_91
action_105 (83) = happyReduce_91
action_105 (84) = happyReduce_91
action_105 (85) = happyReduce_91
action_105 (86) = happyReduce_91
action_105 _ = happyReduce_41

action_106 (44) = happyShift action_109
action_106 _ = happyReduce_51

action_107 (45) = happyShift action_108
action_107 _ = happyFail

action_108 (46) = happyShift action_167
action_108 _ = happyFail

action_109 (44) = happyShift action_98
action_109 (57) = happyShift action_99
action_109 (60) = happyShift action_100
action_109 (61) = happyShift action_101
action_109 (75) = happyShift action_102
action_109 (76) = happyShift action_103
action_109 (77) = happyShift action_104
action_109 (78) = happyShift action_105
action_109 (79) = happyShift action_106
action_109 (80) = happyShift action_57
action_109 (81) = happyShift action_58
action_109 (20) = happyGoto action_164
action_109 (21) = happyGoto action_94
action_109 (22) = happyGoto action_95
action_109 (31) = happyGoto action_165
action_109 (32) = happyGoto action_166
action_109 (33) = happyGoto action_96
action_109 (35) = happyGoto action_97
action_109 _ = happyReduce_85

action_110 (44) = happyShift action_162
action_110 (48) = happyShift action_163
action_110 _ = happyFail

action_111 (45) = happyShift action_161
action_111 _ = happyFail

action_112 (45) = happyShift action_160
action_112 (48) = happyShift action_59
action_112 _ = happyFail

action_113 (42) = happyShift action_114
action_113 _ = happyReduce_96

action_114 (79) = happyShift action_159
action_114 _ = happyFail

action_115 (60) = happyShift action_146
action_115 (61) = happyShift action_147
action_115 (75) = happyShift action_148
action_115 (76) = happyShift action_149
action_115 (77) = happyShift action_150
action_115 (78) = happyShift action_151
action_115 (79) = happyShift action_152
action_115 (80) = happyShift action_57
action_115 (81) = happyShift action_58
action_115 (22) = happyGoto action_144
action_115 (33) = happyGoto action_158
action_115 (35) = happyGoto action_97
action_115 _ = happyFail

action_116 (60) = happyShift action_146
action_116 (61) = happyShift action_147
action_116 (75) = happyShift action_148
action_116 (76) = happyShift action_149
action_116 (77) = happyShift action_150
action_116 (78) = happyShift action_151
action_116 (79) = happyShift action_152
action_116 (80) = happyShift action_57
action_116 (81) = happyShift action_58
action_116 (22) = happyGoto action_144
action_116 (33) = happyGoto action_157
action_116 (35) = happyGoto action_97
action_116 _ = happyFail

action_117 (60) = happyShift action_146
action_117 (61) = happyShift action_147
action_117 (75) = happyShift action_148
action_117 (76) = happyShift action_149
action_117 (77) = happyShift action_150
action_117 (78) = happyShift action_151
action_117 (79) = happyShift action_152
action_117 (80) = happyShift action_57
action_117 (81) = happyShift action_58
action_117 (22) = happyGoto action_144
action_117 (33) = happyGoto action_156
action_117 (35) = happyGoto action_97
action_117 _ = happyFail

action_118 (60) = happyShift action_146
action_118 (61) = happyShift action_147
action_118 (75) = happyShift action_148
action_118 (76) = happyShift action_149
action_118 (77) = happyShift action_150
action_118 (78) = happyShift action_151
action_118 (79) = happyShift action_152
action_118 (80) = happyShift action_57
action_118 (81) = happyShift action_58
action_118 (22) = happyGoto action_144
action_118 (33) = happyGoto action_155
action_118 (35) = happyGoto action_97
action_118 _ = happyFail

action_119 (60) = happyShift action_146
action_119 (61) = happyShift action_147
action_119 (75) = happyShift action_148
action_119 (76) = happyShift action_149
action_119 (77) = happyShift action_150
action_119 (78) = happyShift action_151
action_119 (79) = happyShift action_152
action_119 (80) = happyShift action_57
action_119 (81) = happyShift action_58
action_119 (22) = happyGoto action_144
action_119 (33) = happyGoto action_154
action_119 (35) = happyGoto action_97
action_119 _ = happyFail

action_120 (60) = happyShift action_146
action_120 (61) = happyShift action_147
action_120 (75) = happyShift action_148
action_120 (76) = happyShift action_149
action_120 (77) = happyShift action_150
action_120 (78) = happyShift action_151
action_120 (79) = happyShift action_152
action_120 (80) = happyShift action_57
action_120 (81) = happyShift action_58
action_120 (22) = happyGoto action_144
action_120 (33) = happyGoto action_153
action_120 (35) = happyGoto action_97
action_120 _ = happyFail

action_121 (60) = happyShift action_146
action_121 (61) = happyShift action_147
action_121 (75) = happyShift action_148
action_121 (76) = happyShift action_149
action_121 (77) = happyShift action_150
action_121 (78) = happyShift action_151
action_121 (79) = happyShift action_152
action_121 (80) = happyShift action_57
action_121 (81) = happyShift action_58
action_121 (22) = happyGoto action_144
action_121 (33) = happyGoto action_145
action_121 (35) = happyGoto action_97
action_121 _ = happyFail

action_122 _ = happyReduce_49

action_123 _ = happyReduce_50

action_124 (79) = happyShift action_143
action_124 _ = happyFail

action_125 (44) = happyShift action_98
action_125 (57) = happyShift action_99
action_125 (60) = happyShift action_100
action_125 (61) = happyShift action_101
action_125 (75) = happyShift action_102
action_125 (76) = happyShift action_103
action_125 (77) = happyShift action_104
action_125 (78) = happyShift action_105
action_125 (79) = happyShift action_106
action_125 (80) = happyShift action_57
action_125 (81) = happyShift action_58
action_125 (20) = happyGoto action_142
action_125 (21) = happyGoto action_94
action_125 (22) = happyGoto action_95
action_125 (33) = happyGoto action_96
action_125 (35) = happyGoto action_97
action_125 _ = happyFail

action_126 (44) = happyShift action_98
action_126 (57) = happyShift action_99
action_126 (60) = happyShift action_100
action_126 (61) = happyShift action_101
action_126 (75) = happyShift action_102
action_126 (76) = happyShift action_103
action_126 (77) = happyShift action_104
action_126 (78) = happyShift action_105
action_126 (79) = happyShift action_106
action_126 (80) = happyShift action_57
action_126 (81) = happyShift action_58
action_126 (20) = happyGoto action_141
action_126 (21) = happyGoto action_94
action_126 (22) = happyGoto action_95
action_126 (33) = happyGoto action_96
action_126 (35) = happyGoto action_97
action_126 _ = happyFail

action_127 _ = happyReduce_24

action_128 (41) = happyShift action_134
action_128 (44) = happyShift action_98
action_128 (46) = happyShift action_135
action_128 (57) = happyShift action_99
action_128 (58) = happyShift action_136
action_128 (60) = happyShift action_100
action_128 (61) = happyShift action_101
action_128 (62) = happyShift action_137
action_128 (72) = happyShift action_138
action_128 (73) = happyShift action_139
action_128 (74) = happyShift action_140
action_128 (75) = happyShift action_102
action_128 (76) = happyShift action_103
action_128 (77) = happyShift action_104
action_128 (78) = happyShift action_105
action_128 (79) = happyShift action_106
action_128 (80) = happyShift action_57
action_128 (81) = happyShift action_58
action_128 (20) = happyGoto action_129
action_128 (21) = happyGoto action_94
action_128 (22) = happyGoto action_95
action_128 (23) = happyGoto action_130
action_128 (24) = happyGoto action_131
action_128 (25) = happyGoto action_132
action_128 (33) = happyGoto action_96
action_128 (34) = happyGoto action_133
action_128 (35) = happyGoto action_113
action_128 _ = happyReduce_57

action_129 (41) = happyShift action_187
action_129 _ = happyFail

action_130 (47) = happyShift action_186
action_130 _ = happyFail

action_131 _ = happyReduce_72

action_132 (41) = happyShift action_134
action_132 (44) = happyShift action_98
action_132 (46) = happyShift action_135
action_132 (57) = happyShift action_99
action_132 (58) = happyShift action_136
action_132 (60) = happyShift action_100
action_132 (61) = happyShift action_101
action_132 (62) = happyShift action_137
action_132 (72) = happyShift action_138
action_132 (73) = happyShift action_139
action_132 (74) = happyShift action_140
action_132 (75) = happyShift action_102
action_132 (76) = happyShift action_103
action_132 (77) = happyShift action_104
action_132 (78) = happyShift action_105
action_132 (79) = happyShift action_106
action_132 (80) = happyShift action_57
action_132 (81) = happyShift action_58
action_132 (20) = happyGoto action_129
action_132 (21) = happyGoto action_94
action_132 (22) = happyGoto action_95
action_132 (23) = happyGoto action_185
action_132 (24) = happyGoto action_131
action_132 (25) = happyGoto action_132
action_132 (33) = happyGoto action_96
action_132 (34) = happyGoto action_133
action_132 (35) = happyGoto action_113
action_132 _ = happyReduce_57

action_133 (48) = happyShift action_59
action_133 (79) = happyShift action_184
action_133 _ = happyFail

action_134 _ = happyReduce_71

action_135 (41) = happyShift action_134
action_135 (44) = happyShift action_98
action_135 (46) = happyShift action_135
action_135 (57) = happyShift action_99
action_135 (58) = happyShift action_136
action_135 (60) = happyShift action_100
action_135 (61) = happyShift action_101
action_135 (62) = happyShift action_137
action_135 (72) = happyShift action_138
action_135 (73) = happyShift action_139
action_135 (74) = happyShift action_140
action_135 (75) = happyShift action_102
action_135 (76) = happyShift action_103
action_135 (77) = happyShift action_104
action_135 (78) = happyShift action_105
action_135 (79) = happyShift action_106
action_135 (80) = happyShift action_57
action_135 (81) = happyShift action_58
action_135 (20) = happyGoto action_129
action_135 (21) = happyGoto action_94
action_135 (22) = happyGoto action_95
action_135 (23) = happyGoto action_183
action_135 (24) = happyGoto action_131
action_135 (25) = happyGoto action_132
action_135 (33) = happyGoto action_96
action_135 (34) = happyGoto action_133
action_135 (35) = happyGoto action_113
action_135 _ = happyReduce_57

action_136 (44) = happyShift action_182
action_136 _ = happyFail

action_137 (41) = happyShift action_181
action_137 (44) = happyShift action_98
action_137 (57) = happyShift action_99
action_137 (60) = happyShift action_100
action_137 (61) = happyShift action_101
action_137 (75) = happyShift action_102
action_137 (76) = happyShift action_103
action_137 (77) = happyShift action_104
action_137 (78) = happyShift action_105
action_137 (79) = happyShift action_106
action_137 (80) = happyShift action_57
action_137 (81) = happyShift action_58
action_137 (20) = happyGoto action_180
action_137 (21) = happyGoto action_94
action_137 (22) = happyGoto action_95
action_137 (33) = happyGoto action_96
action_137 (35) = happyGoto action_97
action_137 _ = happyFail

action_138 (80) = happyShift action_57
action_138 (81) = happyShift action_58
action_138 (34) = happyGoto action_179
action_138 (35) = happyGoto action_56
action_138 _ = happyFail

action_139 (44) = happyShift action_178
action_139 _ = happyFail

action_140 (44) = happyShift action_177
action_140 _ = happyFail

action_141 _ = happyReduce_46

action_142 (49) = happyShift action_176
action_142 _ = happyFail

action_143 (44) = happyShift action_175
action_143 _ = happyReduce_52

action_144 (42) = happyShift action_124
action_144 (48) = happyShift action_125
action_144 _ = happyReduce_89

action_145 _ = happyReduce_48

action_146 _ = happyReduce_93

action_147 _ = happyReduce_94

action_148 _ = happyReduce_95

action_149 _ = happyReduce_92

action_150 _ = happyReduce_90

action_151 _ = happyReduce_91

action_152 _ = happyReduce_51

action_153 _ = happyReduce_47

action_154 _ = happyReduce_29

action_155 _ = happyReduce_33

action_156 _ = happyReduce_32

action_157 _ = happyReduce_31

action_158 _ = happyReduce_30

action_159 (44) = happyShift action_174
action_159 _ = happyReduce_55

action_160 (44) = happyShift action_98
action_160 (57) = happyShift action_99
action_160 (60) = happyShift action_100
action_160 (61) = happyShift action_101
action_160 (75) = happyShift action_102
action_160 (76) = happyShift action_103
action_160 (77) = happyShift action_104
action_160 (78) = happyShift action_105
action_160 (79) = happyShift action_106
action_160 (80) = happyShift action_57
action_160 (81) = happyShift action_58
action_160 (20) = happyGoto action_173
action_160 (21) = happyGoto action_94
action_160 (22) = happyGoto action_95
action_160 (33) = happyGoto action_96
action_160 (35) = happyGoto action_97
action_160 _ = happyFail

action_161 _ = happyReduce_44

action_162 (44) = happyShift action_98
action_162 (57) = happyShift action_99
action_162 (60) = happyShift action_100
action_162 (61) = happyShift action_101
action_162 (75) = happyShift action_102
action_162 (76) = happyShift action_103
action_162 (77) = happyShift action_104
action_162 (78) = happyShift action_105
action_162 (79) = happyShift action_106
action_162 (80) = happyShift action_57
action_162 (81) = happyShift action_58
action_162 (20) = happyGoto action_164
action_162 (21) = happyGoto action_94
action_162 (22) = happyGoto action_95
action_162 (31) = happyGoto action_172
action_162 (32) = happyGoto action_166
action_162 (33) = happyGoto action_96
action_162 (35) = happyGoto action_97
action_162 _ = happyReduce_85

action_163 (44) = happyShift action_98
action_163 (57) = happyShift action_99
action_163 (60) = happyShift action_100
action_163 (61) = happyShift action_101
action_163 (75) = happyShift action_102
action_163 (76) = happyShift action_103
action_163 (77) = happyShift action_104
action_163 (78) = happyShift action_105
action_163 (79) = happyShift action_106
action_163 (80) = happyShift action_57
action_163 (81) = happyShift action_58
action_163 (20) = happyGoto action_164
action_163 (21) = happyGoto action_94
action_163 (22) = happyGoto action_95
action_163 (31) = happyGoto action_171
action_163 (32) = happyGoto action_166
action_163 (33) = happyGoto action_96
action_163 (35) = happyGoto action_97
action_163 _ = happyReduce_85

action_164 (43) = happyShift action_170
action_164 _ = happyReduce_87

action_165 (45) = happyShift action_169
action_165 _ = happyFail

action_166 _ = happyReduce_86

action_167 (41) = happyShift action_134
action_167 (44) = happyShift action_98
action_167 (46) = happyShift action_135
action_167 (57) = happyShift action_99
action_167 (58) = happyShift action_136
action_167 (60) = happyShift action_100
action_167 (61) = happyShift action_101
action_167 (62) = happyShift action_137
action_167 (72) = happyShift action_138
action_167 (73) = happyShift action_139
action_167 (74) = happyShift action_140
action_167 (75) = happyShift action_102
action_167 (76) = happyShift action_103
action_167 (77) = happyShift action_104
action_167 (78) = happyShift action_105
action_167 (79) = happyShift action_106
action_167 (80) = happyShift action_57
action_167 (81) = happyShift action_58
action_167 (20) = happyGoto action_129
action_167 (21) = happyGoto action_94
action_167 (22) = happyGoto action_95
action_167 (23) = happyGoto action_168
action_167 (24) = happyGoto action_131
action_167 (25) = happyGoto action_132
action_167 (33) = happyGoto action_96
action_167 (34) = happyGoto action_133
action_167 (35) = happyGoto action_113
action_167 _ = happyReduce_57

action_168 (47) = happyShift action_202
action_168 _ = happyFail

action_169 _ = happyReduce_28

action_170 (44) = happyShift action_98
action_170 (57) = happyShift action_99
action_170 (60) = happyShift action_100
action_170 (61) = happyShift action_101
action_170 (75) = happyShift action_102
action_170 (76) = happyShift action_103
action_170 (77) = happyShift action_104
action_170 (78) = happyShift action_105
action_170 (79) = happyShift action_106
action_170 (80) = happyShift action_57
action_170 (81) = happyShift action_58
action_170 (20) = happyGoto action_164
action_170 (21) = happyGoto action_94
action_170 (22) = happyGoto action_95
action_170 (32) = happyGoto action_201
action_170 (33) = happyGoto action_96
action_170 (35) = happyGoto action_97
action_170 _ = happyFail

action_171 (49) = happyShift action_200
action_171 _ = happyFail

action_172 (45) = happyShift action_199
action_172 _ = happyFail

action_173 _ = happyReduce_45

action_174 (44) = happyShift action_98
action_174 (57) = happyShift action_99
action_174 (60) = happyShift action_100
action_174 (61) = happyShift action_101
action_174 (75) = happyShift action_102
action_174 (76) = happyShift action_103
action_174 (77) = happyShift action_104
action_174 (78) = happyShift action_105
action_174 (79) = happyShift action_106
action_174 (80) = happyShift action_57
action_174 (81) = happyShift action_58
action_174 (20) = happyGoto action_164
action_174 (21) = happyGoto action_94
action_174 (22) = happyGoto action_95
action_174 (31) = happyGoto action_198
action_174 (32) = happyGoto action_166
action_174 (33) = happyGoto action_96
action_174 (35) = happyGoto action_97
action_174 _ = happyReduce_85

action_175 (44) = happyShift action_98
action_175 (57) = happyShift action_99
action_175 (60) = happyShift action_100
action_175 (61) = happyShift action_101
action_175 (75) = happyShift action_102
action_175 (76) = happyShift action_103
action_175 (77) = happyShift action_104
action_175 (78) = happyShift action_105
action_175 (79) = happyShift action_106
action_175 (80) = happyShift action_57
action_175 (81) = happyShift action_58
action_175 (20) = happyGoto action_164
action_175 (21) = happyGoto action_94
action_175 (22) = happyGoto action_95
action_175 (31) = happyGoto action_197
action_175 (32) = happyGoto action_166
action_175 (33) = happyGoto action_96
action_175 (35) = happyGoto action_97
action_175 _ = happyReduce_85

action_176 _ = happyReduce_53

action_177 (44) = happyShift action_98
action_177 (57) = happyShift action_99
action_177 (60) = happyShift action_100
action_177 (61) = happyShift action_101
action_177 (75) = happyShift action_102
action_177 (76) = happyShift action_103
action_177 (77) = happyShift action_104
action_177 (78) = happyShift action_105
action_177 (79) = happyShift action_106
action_177 (80) = happyShift action_57
action_177 (81) = happyShift action_58
action_177 (20) = happyGoto action_196
action_177 (21) = happyGoto action_94
action_177 (22) = happyGoto action_95
action_177 (33) = happyGoto action_96
action_177 (35) = happyGoto action_97
action_177 _ = happyFail

action_178 (79) = happyShift action_195
action_178 (80) = happyShift action_57
action_178 (81) = happyShift action_58
action_178 (34) = happyGoto action_194
action_178 (35) = happyGoto action_56
action_178 _ = happyFail

action_179 (48) = happyShift action_59
action_179 (79) = happyShift action_193
action_179 _ = happyFail

action_180 (41) = happyShift action_192
action_180 _ = happyFail

action_181 _ = happyReduce_66

action_182 (44) = happyShift action_98
action_182 (57) = happyShift action_99
action_182 (60) = happyShift action_100
action_182 (61) = happyShift action_101
action_182 (75) = happyShift action_102
action_182 (76) = happyShift action_103
action_182 (77) = happyShift action_104
action_182 (78) = happyShift action_105
action_182 (79) = happyShift action_106
action_182 (80) = happyShift action_57
action_182 (81) = happyShift action_58
action_182 (20) = happyGoto action_191
action_182 (21) = happyGoto action_94
action_182 (22) = happyGoto action_95
action_182 (33) = happyGoto action_96
action_182 (35) = happyGoto action_97
action_182 _ = happyFail

action_183 (47) = happyShift action_190
action_183 _ = happyFail

action_184 (41) = happyShift action_188
action_184 (50) = happyShift action_189
action_184 _ = happyFail

action_185 _ = happyReduce_58

action_186 _ = happyReduce_27

action_187 _ = happyReduce_60

action_188 _ = happyReduce_65

action_189 (44) = happyShift action_98
action_189 (57) = happyShift action_99
action_189 (60) = happyShift action_100
action_189 (61) = happyShift action_101
action_189 (75) = happyShift action_102
action_189 (76) = happyShift action_103
action_189 (77) = happyShift action_104
action_189 (78) = happyShift action_105
action_189 (79) = happyShift action_106
action_189 (80) = happyShift action_57
action_189 (81) = happyShift action_58
action_189 (20) = happyGoto action_210
action_189 (21) = happyGoto action_94
action_189 (22) = happyGoto action_95
action_189 (33) = happyGoto action_96
action_189 (35) = happyGoto action_97
action_189 _ = happyFail

action_190 _ = happyReduce_59

action_191 (45) = happyShift action_209
action_191 _ = happyFail

action_192 _ = happyReduce_67

action_193 (50) = happyShift action_208
action_193 _ = happyFail

action_194 (48) = happyShift action_59
action_194 (79) = happyShift action_207
action_194 _ = happyFail

action_195 (50) = happyShift action_206
action_195 _ = happyFail

action_196 (45) = happyShift action_205
action_196 _ = happyFail

action_197 (45) = happyShift action_204
action_197 _ = happyFail

action_198 (45) = happyShift action_203
action_198 _ = happyFail

action_199 _ = happyReduce_42

action_200 _ = happyReduce_43

action_201 _ = happyReduce_88

action_202 _ = happyReduce_25

action_203 _ = happyReduce_56

action_204 _ = happyReduce_54

action_205 (46) = happyShift action_135
action_205 (24) = happyGoto action_216
action_205 _ = happyFail

action_206 (44) = happyShift action_98
action_206 (57) = happyShift action_99
action_206 (60) = happyShift action_100
action_206 (61) = happyShift action_101
action_206 (75) = happyShift action_102
action_206 (76) = happyShift action_103
action_206 (77) = happyShift action_104
action_206 (78) = happyShift action_105
action_206 (79) = happyShift action_106
action_206 (80) = happyShift action_57
action_206 (81) = happyShift action_58
action_206 (20) = happyGoto action_215
action_206 (21) = happyGoto action_94
action_206 (22) = happyGoto action_95
action_206 (33) = happyGoto action_96
action_206 (35) = happyGoto action_97
action_206 _ = happyFail

action_207 (50) = happyShift action_214
action_207 _ = happyFail

action_208 (44) = happyShift action_98
action_208 (57) = happyShift action_99
action_208 (60) = happyShift action_100
action_208 (61) = happyShift action_101
action_208 (75) = happyShift action_102
action_208 (76) = happyShift action_103
action_208 (77) = happyShift action_104
action_208 (78) = happyShift action_105
action_208 (79) = happyShift action_106
action_208 (80) = happyShift action_57
action_208 (81) = happyShift action_58
action_208 (20) = happyGoto action_213
action_208 (21) = happyGoto action_94
action_208 (22) = happyGoto action_95
action_208 (33) = happyGoto action_96
action_208 (35) = happyGoto action_97
action_208 _ = happyFail

action_209 (46) = happyShift action_135
action_209 (24) = happyGoto action_212
action_209 _ = happyFail

action_210 (41) = happyShift action_211
action_210 _ = happyFail

action_211 _ = happyReduce_64

action_212 (59) = happyShift action_220
action_212 _ = happyReduce_61

action_213 (41) = happyShift action_219
action_213 _ = happyFail

action_214 (44) = happyShift action_98
action_214 (57) = happyShift action_99
action_214 (60) = happyShift action_100
action_214 (61) = happyShift action_101
action_214 (75) = happyShift action_102
action_214 (76) = happyShift action_103
action_214 (77) = happyShift action_104
action_214 (78) = happyShift action_105
action_214 (79) = happyShift action_106
action_214 (80) = happyShift action_57
action_214 (81) = happyShift action_58
action_214 (20) = happyGoto action_218
action_214 (21) = happyGoto action_94
action_214 (22) = happyGoto action_95
action_214 (33) = happyGoto action_96
action_214 (35) = happyGoto action_97
action_214 _ = happyFail

action_215 (41) = happyShift action_217
action_215 _ = happyFail

action_216 _ = happyReduce_68

action_217 (44) = happyShift action_98
action_217 (57) = happyShift action_99
action_217 (60) = happyShift action_100
action_217 (61) = happyShift action_101
action_217 (75) = happyShift action_102
action_217 (76) = happyShift action_103
action_217 (77) = happyShift action_104
action_217 (78) = happyShift action_105
action_217 (79) = happyShift action_106
action_217 (80) = happyShift action_57
action_217 (81) = happyShift action_58
action_217 (20) = happyGoto action_223
action_217 (21) = happyGoto action_94
action_217 (22) = happyGoto action_95
action_217 (33) = happyGoto action_96
action_217 (35) = happyGoto action_97
action_217 _ = happyFail

action_218 (41) = happyShift action_222
action_218 _ = happyFail

action_219 _ = happyReduce_63

action_220 (41) = happyShift action_134
action_220 (44) = happyShift action_98
action_220 (46) = happyShift action_135
action_220 (57) = happyShift action_99
action_220 (58) = happyShift action_136
action_220 (60) = happyShift action_100
action_220 (61) = happyShift action_101
action_220 (62) = happyShift action_137
action_220 (72) = happyShift action_138
action_220 (73) = happyShift action_139
action_220 (74) = happyShift action_140
action_220 (75) = happyShift action_102
action_220 (76) = happyShift action_103
action_220 (77) = happyShift action_104
action_220 (78) = happyShift action_105
action_220 (79) = happyShift action_106
action_220 (80) = happyShift action_57
action_220 (81) = happyShift action_58
action_220 (20) = happyGoto action_129
action_220 (21) = happyGoto action_94
action_220 (22) = happyGoto action_95
action_220 (24) = happyGoto action_131
action_220 (25) = happyGoto action_221
action_220 (33) = happyGoto action_96
action_220 (34) = happyGoto action_133
action_220 (35) = happyGoto action_113
action_220 _ = happyFail

action_221 _ = happyReduce_62

action_222 (44) = happyShift action_98
action_222 (57) = happyShift action_99
action_222 (60) = happyShift action_100
action_222 (61) = happyShift action_101
action_222 (75) = happyShift action_102
action_222 (76) = happyShift action_103
action_222 (77) = happyShift action_104
action_222 (78) = happyShift action_105
action_222 (79) = happyShift action_106
action_222 (80) = happyShift action_57
action_222 (81) = happyShift action_58
action_222 (20) = happyGoto action_225
action_222 (21) = happyGoto action_94
action_222 (22) = happyGoto action_95
action_222 (33) = happyGoto action_96
action_222 (35) = happyGoto action_97
action_222 _ = happyFail

action_223 (41) = happyShift action_224
action_223 _ = happyFail

action_224 (44) = happyShift action_98
action_224 (57) = happyShift action_99
action_224 (60) = happyShift action_100
action_224 (61) = happyShift action_101
action_224 (75) = happyShift action_102
action_224 (76) = happyShift action_103
action_224 (77) = happyShift action_104
action_224 (78) = happyShift action_105
action_224 (79) = happyShift action_106
action_224 (80) = happyShift action_57
action_224 (81) = happyShift action_58
action_224 (20) = happyGoto action_227
action_224 (21) = happyGoto action_94
action_224 (22) = happyGoto action_95
action_224 (33) = happyGoto action_96
action_224 (35) = happyGoto action_97
action_224 _ = happyFail

action_225 (41) = happyShift action_226
action_225 _ = happyFail

action_226 (44) = happyShift action_98
action_226 (57) = happyShift action_99
action_226 (60) = happyShift action_100
action_226 (61) = happyShift action_101
action_226 (75) = happyShift action_102
action_226 (76) = happyShift action_103
action_226 (77) = happyShift action_104
action_226 (78) = happyShift action_105
action_226 (79) = happyShift action_106
action_226 (80) = happyShift action_57
action_226 (81) = happyShift action_58
action_226 (20) = happyGoto action_229
action_226 (21) = happyGoto action_94
action_226 (22) = happyGoto action_95
action_226 (33) = happyGoto action_96
action_226 (35) = happyGoto action_97
action_226 _ = happyFail

action_227 (45) = happyShift action_228
action_227 _ = happyFail

action_228 (46) = happyShift action_135
action_228 (24) = happyGoto action_231
action_228 _ = happyFail

action_229 (45) = happyShift action_230
action_229 _ = happyFail

action_230 (46) = happyShift action_135
action_230 (24) = happyGoto action_232
action_230 _ = happyFail

action_231 _ = happyReduce_69

action_232 _ = happyReduce_70

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn26  happy_var_3)
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
	(HappyAbsSyn27  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_7) `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	(HappyTerminal (TokenClassType happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (let attr = if happy_var_3 then ["static"] else []
            in  Class attr happy_var_5 happy_var_6 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 7 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
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
happyReduction_18 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  14 happyReduction_19
happyReduction_19  =  HappyAbsSyn14
		 ([]
	)

happyReduce_20 = happySpecReduce_2  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 _
	(HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn16
		 (\attr -> FieldDecl attr happy_var_1 happy_var_2 Nothing
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 16 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (\attr -> FieldDecl attr happy_var_1 happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 8 17 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (\attr -> MethodDecl attr happy_var_1 happy_var_2 happy_var_4 (toStmt happy_var_7)
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 6 18 happyReduction_26
happyReduction_26 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (AbstractMethodDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 7 19 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenClassType happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (ConstrDecl happy_var_1 happy_var_3 (toStmt happy_var_6)
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 20 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Prim happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  20 happyReduction_29
happyReduction_29 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "==" [happy_var_1, happy_var_3]
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  20 happyReduction_30
happyReduction_30 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "+" [happy_var_1, happy_var_3]
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  20 happyReduction_31
happyReduction_31 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "-" [happy_var_1, happy_var_3]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "*" [happy_var_1, happy_var_3]
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn20
		 (Prim "/" [happy_var_1, happy_var_3]
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  21 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn21
		 (ConstTrue
	)

happyReduce_37 = happySpecReduce_1  21 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn21
		 (ConstFalse
	)

happyReduce_38 = happySpecReduce_1  21 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn21
		 (ConstNull
	)

happyReduce_39 = happySpecReduce_1  21 happyReduction_39
happyReduction_39 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn21
		 (ConstNum happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  21 happyReduction_40
happyReduction_40 (HappyTerminal (TokenLit happy_var_1))
	 =  HappyAbsSyn21
		 (ConstLit happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  21 happyReduction_41
happyReduction_41 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn21
		 (ConstChar happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 5 21 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (New (TypeName happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 5 21 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (New (ArrayTypeName (TypeName happy_var_2)) happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 21 happyReduction_45
happyReduction_45 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Cast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  21 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  21 happyReduction_47
happyReduction_47 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn21
		 (Prim "<" [happy_var_1, happy_var_3]
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  21 happyReduction_48
happyReduction_48 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn21
		 (Prim "!=" [happy_var_1, happy_var_3]
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  21 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn21
		 (Prim "++" [happy_var_1, happy_var_1]
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  21 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn21
		 (Prim "--" [happy_var_1, happy_var_1]
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  22 happyReduction_51
happyReduction_51 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn22
		 (Var happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  22 happyReduction_52
happyReduction_52 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (Field happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 4 22 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Prim "[]" [happy_var_1, happy_var_3]
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 6 22 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Invoke happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_3  22 happyReduction_55
happyReduction_55 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn22
		 (StaticField (TypeName happy_var_1) happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happyReduce 6 22 happyReduction_56
happyReduction_56 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (StaticInvoke (TypeName happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_0  23 happyReduction_57
happyReduction_57  =  HappyAbsSyn23
		 ([]
	)

happyReduce_58 = happySpecReduce_2  23 happyReduction_58
happyReduction_58 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  24 happyReduction_59
happyReduction_59 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Block $ toStmt happy_var_2
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  25 happyReduction_60
happyReduction_60 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn25
		 (Expr happy_var_1
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 25 happyReduction_61
happyReduction_61 ((HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Ite happy_var_3 happy_var_5 NoStmt
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 7 25 happyReduction_62
happyReduction_62 ((HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Ite happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 6 25 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (LocalVarDecl happy_var_2 happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 5 25 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (LocalVarDecl happy_var_1 happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  25 happyReduction_65
happyReduction_65 _
	(HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn25
		 (LocalVarDecl happy_var_1 happy_var_2 Nothing
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  25 happyReduction_66
happyReduction_66 _
	_
	 =  HappyAbsSyn25
		 (Return Nothing
	)

happyReduce_67 = happySpecReduce_3  25 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Return (Just happy_var_2)
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happyReduce 5 25 happyReduction_68
happyReduction_68 ((HappyAbsSyn24  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_69 = happyReduce 11 25 happyReduction_69
happyReduction_69 ((HappyAbsSyn24  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (For Nothing happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_70 = happyReduce 12 25 happyReduction_70
happyReduction_70 ((HappyAbsSyn24  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_4)) `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (For (Just happy_var_3) happy_var_4 happy_var_6 happy_var_8 happy_var_10 happy_var_12
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_1  25 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn25
		 (NoStmt
	)

happyReduce_72 = happySpecReduce_1  25 happyReduction_72
happyReduction_72 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  26 happyReduction_73
happyReduction_73 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn26
		 ([happy_var_1]
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  26 happyReduction_74
happyReduction_74 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 : happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  27 happyReduction_75
happyReduction_75  =  HappyAbsSyn27
		 ([]
	)

happyReduce_76 = happyReduce 5 27 happyReduction_76
happyReduction_76 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (let attr = if happy_var_3 then ["static"] else [] in
            (happy_var_4 attr) : happy_var_5
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 5 27 happyReduction_77
happyReduction_77 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (let attr = if happy_var_3 then ["static"] else [] in
            (happy_var_4 attr) : happy_var_5
	) `HappyStk` happyRest

happyReduce_78 = happyReduce 5 27 happyReduction_78
happyReduction_78 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (happy_var_4 : happy_var_5
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_0  28 happyReduction_79
happyReduction_79  =  HappyAbsSyn28
		 ([]
	)

happyReduce_80 = happyReduce 4 28 happyReduction_80
happyReduction_80 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (happy_var_3 : happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happySpecReduce_0  29 happyReduction_81
happyReduction_81  =  HappyAbsSyn29
		 ([]
	)

happyReduce_82 = happySpecReduce_1  29 happyReduction_82
happyReduction_82 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  30 happyReduction_83
happyReduction_83 (HappyTerminal (TokenVar happy_var_3))
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn30
		 ([(happy_var_2, happy_var_3)]
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happyReduce 5 30 happyReduction_84
happyReduction_84 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 ((happy_var_2, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_0  31 happyReduction_85
happyReduction_85  =  HappyAbsSyn31
		 ([]
	)

happyReduce_86 = happySpecReduce_1  31 happyReduction_86
happyReduction_86 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  32 happyReduction_87
happyReduction_87 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn32
		 ([happy_var_1]
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  32 happyReduction_88
happyReduction_88 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 : happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  33 happyReduction_89
happyReduction_89 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  33 happyReduction_90
happyReduction_90 (HappyTerminal (TokenLit happy_var_1))
	 =  HappyAbsSyn33
		 (ConstLit happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  33 happyReduction_91
happyReduction_91 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn33
		 (ConstChar happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  33 happyReduction_92
happyReduction_92 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn33
		 (ConstNum happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  33 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn33
		 (ConstTrue
	)

happyReduce_94 = happySpecReduce_1  33 happyReduction_94
happyReduction_94 _
	 =  HappyAbsSyn33
		 (ConstFalse
	)

happyReduce_95 = happySpecReduce_1  33 happyReduction_95
happyReduction_95 _
	 =  HappyAbsSyn33
		 (ConstNull
	)

happyReduce_96 = happySpecReduce_1  34 happyReduction_96
happyReduction_96 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn34
		 (TypeName happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  34 happyReduction_97
happyReduction_97 _
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (ArrayTypeName happy_var_1
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  35 happyReduction_98
happyReduction_98 (HappyTerminal (TokenClassType happy_var_1))
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  35 happyReduction_99
happyReduction_99 (HappyTerminal (TokenPrimitiveType happy_var_1))
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_0  36 happyReduction_100
happyReduction_100  =  HappyAbsSyn36
		 (False
	)

happyReduce_101 = happySpecReduce_1  36 happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn36
		 (True
	)

happyReduce_102 = happySpecReduce_1  37 happyReduction_102
happyReduction_102 _
	 =  HappyAbsSyn37
		 (1
	)

happyReduce_103 = happySpecReduce_1  37 happyReduction_103
happyReduction_103 _
	 =  HappyAbsSyn37
		 (2
	)

happyReduce_104 = happySpecReduce_1  37 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn37
		 (3
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

happyReduce_109 = happySpecReduce_0  40 happyReduction_109
happyReduction_109  =  HappyAbsSyn40
		 (False
	)

happyReduce_110 = happySpecReduce_1  40 happyReduction_110
happyReduction_110 _
	 =  HappyAbsSyn40
		 (True
	)

happyNewToken action sts stk [] =
	action 87 87 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSemiColon -> cont 41;
	TokenDot -> cont 42;
	TokenComma -> cont 43;
	TokenOP -> cont 44;
	TokenCP -> cont 45;
	TokenOB -> cont 46;
	TokenCB -> cont 47;
	TokenOBK -> cont 48;
	TokenCBK -> cont 49;
	TokenEQ -> cont 50;
	TokenPlus -> cont 51;
	TokenMinus -> cont 52;
	TokenStar -> cont 53;
	TokenDiv -> cont 54;
	TokenClass -> cont 55;
	TokenExtends -> cont 56;
	TokenNew -> cont 57;
	TokenIf -> cont 58;
	TokenElse -> cont 59;
	TokenTrue -> cont 60;
	TokenFalse -> cont 61;
	TokenReturn -> cont 62;
	TokenPackage -> cont 63;
	TokenImport -> cont 64;
	TokenPublic -> cont 65;
	TokenProtected -> cont 66;
	TokenPrivate -> cont 67;
	TokenStatic -> cont 68;
	TokenAbstract -> cont 69;
	TokenInterface -> cont 70;
	TokenImplements -> cont 71;
	TokenFinal -> cont 72;
	TokenFor -> cont 73;
	TokenWhile -> cont 74;
	TokenNull -> cont 75;
	TokenNum happy_dollar_dollar -> cont 76;
	TokenLit happy_dollar_dollar -> cont 77;
	TokenChar happy_dollar_dollar -> cont 78;
	TokenVar happy_dollar_dollar -> cont 79;
	TokenClassType happy_dollar_dollar -> cont 80;
	TokenPrimitiveType happy_dollar_dollar -> cont 81;
	TokenEqual -> cont 82;
	TokenLT -> cont 83;
	TokenNotEq -> cont 84;
	TokenInc -> cont 85;
	TokenDec -> cont 86;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

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
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

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

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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

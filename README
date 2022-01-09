# A reference implementation for the following paper
 - Kwanghoon Choi, Byeong-Mo Chang, A Type and Effect System for Activation Flow of Components in Android Programs, Information Processing Letters, 114(11):620-627, November 2014.

### INSTRUCTION

 0. Install Glasgow Haskell Compiler 
     - Avaialable from http://www.haskell.org

 1. Run ghci to start Android analyzer
     - $ ghci
     - Prelude> :l Main
     - *Main> doandroidtest tc_android 

 2. The following shows a sample run.

$ ghci
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude> :l Main
[1 of 7] Compiling AST              ( AST.hs, interpreted )
[2 of 7] Compiling Library          ( Library.hs, interpreted )
[3 of 7] Compiling Subtyping        ( Subtyping.hs, interpreted )
[4 of 7] Compiling TypeCheck        ( TypeCheck.hs, interpreted )
[5 of 7] Compiling Analysis         ( Analysis.hs, interpreted )
[6 of 7] Compiling Parser           ( Parser.hs, interpreted )
[7 of 7] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Parser, Main, AST, TypeCheck, Analysis, Library, Subtyping.
*Main> doandroidtest tc_android
Parsing ./sample/androidgame/Android.java
Parsing ./sample/androidgame/Game.java
Typechecking Activity
Typechecking Intent
Typechecking Main
 - Main.onCreate:2 overrides Activity.onCreate:4
 - Main.onClick:3 overrides Activity.onClick:5
Typechecking Game
 - Game.onCreate:2 overrides Activity.onCreate:4
 - Game.onClick:3 overrides Activity.onClick:5
Typechecking Help
 - Help.onCreate:2 overrides Activity.onCreate:4
 - Help.onClick:3 overrides Activity.onClick:5
Typechecking Score
 - Score.onCreate:2 overrides Activity.onCreate:4
 - Score.onClick:3 overrides Activity.onClick:5
class Activity  {
   Activity() { // m1 ()
      super();
   }
   Intent intent; // f2
   Intent getIntent() { // m3 ()
      return this.intent;
   }
   void onCreate() { // m4 ()
   }
   void onClick(int button) { // m5 (v2)
   }
   void addButton(int button) { // m6 (v2)
      primAddButton(button);
   }
   void startActivity(Intent i) { // m7 (v2)
      primStartActivity(i);
   }
}

class Intent  {
   Intent() { // m1 ()
      super();
   }
   String target; // f2
   Object data; // f3
   void setTarget(String s) { // m4 (v2)
      this.target=s;
   }
   void setData(Object d) { // m5 (v2)
      this.data=d;
   }
   Object getData() { // m6 ()
      return this.data;
   }
}

class Main extends Activity {
   Main() { // m1 ()
      super();
   }
   void onCreate() { // m2 ()
      this.addButton(1);
      this.addButton(2);
      this.addButton(3);
   }
   void onClick(int button) { // m3 (v2)
      if (button==1)
         {
            Intent i=new Intent() /* a1 */ ; // v3
            i.setTarget("Game" /* a2 */ );
            this.startActivity(i);
         }
      
      else
         if (button==2)
            {
               Intent i=new Intent() /* a3 */ ; // v4
               i.setTarget("Score" /* a4 */ );
               this.startActivity(i);
            }
         
         else
            if (button==3)
               {
                  Intent i=new Intent() /* a5 */ ; // v5
                  i.setTarget("Help" /* a6 */ );
                  i.setData("Main" /* a7 */ );
                  this.startActivity(i);
               }
            
            else
               {
               }
            
         
      
   }
}

class Game extends Activity {
   Game() { // m1 ()
      super();
   }
   void onCreate() { // m2 ()
      this.addButton(1);
      this.addButton(2);
      this.addButton(3);
   }
   void onClick(int button) { // m3 (v2)
      if (button==1)
         {
            Intent i=new Intent() /* a1 */ ; // v3
            i.setTarget("Help" /* a2 */ );
            i.setData("Game" /* a3 */ );
            this.startActivity(i);
         }
      
      else
         if (button==2)
            {
               Intent i=new Intent() /* a4 */ ; // v4
               i.setTarget("Score" /* a5 */ );
               this.startActivity(i);
            }
         
         else
            if (button==3)
               {
               }
            
            else
               {
               }
            
         
      
   }
}

class Help extends Activity {
   Help() { // m1 ()
      super();
   }
   void onCreate() { // m2 ()
      this.addButton(1);
   }
   void onClick(int button) { // m3 (v2)
      Intent i=new Intent() /* a1 */ ; // v3
      Intent j=this.getIntent(); // v4
      Object o=j.getData(); // v5
      String t=(String)o; // v6
      i.setTarget(t);
      this.startActivity(i);
   }
}

class Score extends Activity {
   Score() { // m1 ()
      super();
   }
   void onCreate() { // m2 ()
      this.addButton(1);
   }
   void onClick(int button) { // m3 (v2)
      Intent i=new Intent() /* a1 */ ; // v3
      i.setTarget("Main" /* a2 */ );
      this.startActivity(i);
   }
}

Classes: 
 - class Activity
 - class Intent
 - class Main
 - class Game
 - class Help
 - class Score

Classes: 
 - class String
 - class Object

Inheritance: 
 - Activity <: Object
 - Intent <: Object
 - Main <: Activity
 - Game <: Activity
 - Help <: Activity
 - Score <: Activity

Inheritance: 
 - String <: Object

Fields:
 - Activity :  Intent intent
 - Intent :  String target,  Object data
 - Main : 
 - Game : 
 - Help : 
 - Score : 

Fields:
 - System : static PrintStream out

Method Types:
 - Activity,Activity :  () --> Activity
 - Activity,getIntent :  () --> Intent
 - Activity,onCreate :  () --> void
 - Activity,onClick :  (int) --> void
 - Activity,addButton :  (int) --> void
 - Activity,startActivity :  (Intent) --> void
 - Intent,Intent :  () --> Intent
 - Intent,setTarget :  (String) --> void
 - Intent,setData :  (Object) --> void
 - Intent,getData :  () --> Object
 - Main,Main :  () --> Main
 - Main,onCreate :  () --> void
 - Main,onClick :  (int) --> void
 - Game,Game :  () --> Game
 - Game,onCreate :  () --> void
 - Game,onClick :  (int) --> void
 - Help,Help :  () --> Help
 - Help,onCreate :  () --> void
 - Help,onClick :  (int) --> void
 - Score,Score :  () --> Score
 - Score,onCreate :  () --> void
 - Score,onClick :  (int) --> void

Method Types:
 - Object,getClass :  () --> Class<?>
 - Class<?>,getName :  () --> String
 - StringBuilder,StringBuilder :  () --> StringBuilder
 - StringBuilder,append :  (String) --> StringBuilder
 - StringBuilder,append :  (char) --> StringBuilder
 - StringBuilder,setLength :  (int) --> void
 - StringBuilder,toString :  () --> String
 - String,equals :  (String) --> boolean
 - PrintStream,println : static (String) --> void
 - PrintStream,println : static (StringBuilder) --> void

Variable Types:
 - Activity,this,0 : Activity
 - Activity,Activity,1,return,1 : Activity
 - Activity,getIntent,3,return,1 : Intent
 - Activity,onCreate,4,return,1 : void
 - Activity,onClick,5,button,2 : int
 - Activity,onClick,5,return,1 : void
 - Activity,addButton,6,button,2 : int
 - Activity,addButton,6,return,1 : void
 - Activity,startActivity,7,i,2 : Intent
 - Activity,startActivity,7,return,1 : void
 - Intent,this,0 : Intent
 - Intent,Intent,1,return,1 : Intent
 - Intent,setTarget,4,s,2 : String
 - Intent,setTarget,4,return,1 : void
 - Intent,setData,5,d,2 : Object
 - Intent,setData,5,return,1 : void
 - Intent,getData,6,return,1 : Object
 - Main,this,0 : Main
 - Main,Main,1,return,1 : Main
 - Main,onCreate,2,return,1 : void
 - Main,onClick,3,i,3 : Intent
 - Main,onClick,3,i,4 : Intent
 - Main,onClick,3,i,5 : Intent
 - Main,onClick,3,button,2 : int
 - Main,onClick,3,return,1 : void
 - Game,this,0 : Game
 - Game,Game,1,return,1 : Game
 - Game,onCreate,2,return,1 : void
 - Game,onClick,3,i,3 : Intent
 - Game,onClick,3,i,4 : Intent
 - Game,onClick,3,button,2 : int
 - Game,onClick,3,return,1 : void
 - Help,this,0 : Help
 - Help,Help,1,return,1 : Help
 - Help,onCreate,2,return,1 : void
 - Help,onClick,3,i,3 : Intent
 - Help,onClick,3,j,4 : Intent
 - Help,onClick,3,o,5 : Object
 - Help,onClick,3,t,6 : String
 - Help,onClick,3,button,2 : int
 - Help,onClick,3,return,1 : void
 - Score,this,0 : Score
 - Score,Score,1,return,1 : Score
 - Score,onCreate,2,return,1 : void
 - Score,onClick,3,i,3 : Intent
 - Score,onClick,3,button,2 : int
 - Score,onClick,3,return,1 : void

Successfully typechecked...

The initial context: o1
### Initialization Round ###

Activity,Activity,1(o1):
 - { o1 } <= x102
 - null{x103} <: Activity{x102}.intent
 - { o1 } <= x101
 - Object{x106}.Object <: ()--e107-->Object{x106}
 - { o1 } <= x106
 - e107 U {} <= e105
Activity,intent,2(o1):
Activity,getIntent,3(o1):
 - { o1 } <= x102
 - Activity{x102}.intent <: Intent{x110}
 - Intent{x110} <: Intent{x108}
 - {} <= e109
Activity,onCreate,4(o1):
 - { o1 } <= x102
 - {} <= e112
Activity,onClick,5(o1):
 - { o1 } <= x102
 - {} <= e115
Activity,addButton,6(o1):
 - { o1 } <= x102
 - {} U {} U e119 <= e118
Activity,startActivity,7(o1):
 - { o1 } <= x102
 - Intent{x121} ~~~> e124 @ [{o1},ActivitystartActivity,7,1, n/a]
 - {} U {} U e124 <= e123
Intent,Intent,1(o1):
Intent,target,2(o1):
Intent,data,3(o1):
Intent,setTarget,4(o1):
Intent,setData,5(o1):
Intent,getData,6(o1):
Main,Main,1(o1):
 - { o1 } <= x127
 - { o1 } <= x126
 - Activity{x129}.Activity <: ()--e130-->Activity{x129}
 - { o1 } <= x129
 - e130 U {} <= e128
Main,onCreate,2(o1):
 - { o1 } <= x127
 - Main{x127}.addButton <: (int{x133})--e135-->void{x134}
 - Main{x127}.addButton <: (int{x136})--e138-->void{x137}
 - Main{x127}.addButton <: (int{x139})--e141-->void{x140}
 - {} U {} U {} U e135 U {} U {} U {} U e138 U {} U {} U {} U e141 <= e132
Main,onClick,3(o1):
 - { o1 } <= x127
 - { o152 } <= x150
 - Intent{x150}.Intent <: ()--e151-->Intent{x150}
 - Intent{x150} <: Intent{x142}
 - { o154 } <= x153
 - Intent{x142}.setTarget <: (String{x153})--e156-->void{x155}
 - Main{x127}.startActivity <: (Intent{x142})--e158-->void{x157}
 - { o163 } <= x161
 - Intent{x161}.Intent <: ()--e162-->Intent{x161}
 - Intent{x161} <: Intent{x143}
 - { o165 } <= x164
 - Intent{x143}.setTarget <: (String{x164})--e167-->void{x166}
 - Main{x127}.startActivity <: (Intent{x143})--e169-->void{x168}
 - { o174 } <= x172
 - Intent{x172}.Intent <: ()--e173-->Intent{x172}
 - Intent{x172} <: Intent{x144}
 - { o176 } <= x175
 - Intent{x144}.setTarget <: (String{x175})--e178-->void{x177}
 - { o180 } <= x179
 - Intent{x144}.setData <: (String{x179})--e182-->void{x181}
 - Main{x127}.startActivity <: (Intent{x144})--e184-->void{x183}
 - {} U {} U {} U {} U e151 U {} U {} U {} U {} U e156 U {} U {} U {} U e158 U {} U {} U {} U {} U e162 U {} U {} U {} U {} U e167 U {} U {} U {} U e169 U {} U {} U {} U {} U e173 U {} U {} U {} U {} U e178 U {} U {} U {} U e182 U {} U {} U {} U e184 U {} <= e147
Game,Game,1(o1):
Game,onCreate,2(o1):
Game,onClick,3(o1):
Help,Help,1(o1):
Help,onCreate,2(o1):
Help,onClick,3(o1):
Score,Score,1(o1):
Score,onCreate,2(o1):
Score,onClick,3(o1):
Main,onCreate,2(o1):
 - ()--e132-->void{x131} <: ()--e112-->void{x111}
Main,onClick,3(o1):
 - (int{x145})--e147-->void{x146} <: (int{x113})--e115-->void{x114}
Game,onCreate,2(o1):
Game,onClick,3(o1):
Help,onCreate,2(o1):
Help,onClick,3(o1):
Score,onCreate,2(o1):
Score,onClick,3(o1):

### Round 1###

Activity,Activity,1(o180):
Activity,intent,2(o180):
Activity,getIntent,3(o180):
Activity,onCreate,4(o180):
Activity,onClick,5(o180):
Activity,addButton,6(o180):
Activity,startActivity,7(o180):
Intent,Intent,1(o180):
Intent,target,2(o180):
Intent,data,3(o180):
Intent,setTarget,4(o180):
Intent,setData,5(o180):
Intent,getData,6(o180):
Main,Main,1(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,Game,1(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,Help,1(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,Score,1(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Activity,Activity,1(o176):
Activity,intent,2(o176):
Activity,getIntent,3(o176):
Activity,onCreate,4(o176):
Activity,onClick,5(o176):
Activity,addButton,6(o176):
Activity,startActivity,7(o176):
Intent,Intent,1(o176):
Intent,target,2(o176):
Intent,data,3(o176):
Intent,setTarget,4(o176):
Intent,setData,5(o176):
Intent,getData,6(o176):
Main,Main,1(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,Game,1(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,Help,1(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,Score,1(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Activity,Activity,1(o174):
Activity,intent,2(o174):
Activity,getIntent,3(o174):
Activity,onCreate,4(o174):
Activity,onClick,5(o174):
Activity,addButton,6(o174):
Activity,startActivity,7(o174):
Intent,Intent,1(o174):
 - { o174 } <= x186
 - null{x187} <: Intent{x186}.target
 - null{x189} <: Intent{x186}.data
 - { o174 } <= x185
 - Object{x192}.Object <: ()--e193-->Object{x192}
 - { o174 } <= x192
 - e193 U {} <= e191
Intent,target,2(o174):
Intent,data,3(o174):
Intent,setTarget,4(o174):
 - { o174 } <= x186
 - String{x194} <: Intent{x186}.target
 - {  } <= x197
 - {} U {} <= e196
Intent,setData,5(o174):
 - { o174 } <= x186
 - Object{x198} <: Intent{x186}.data
 - {  } <= x201
 - {} U {} <= e200
Intent,getData,6(o174):
 - { o174 } <= x186
 - Intent{x186}.data <: Object{x204}
 - Object{x204} <: Object{x202}
 - {} <= e203
Main,Main,1(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,Game,1(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,Help,1(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,Score,1(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Activity,Activity,1(o165):
Activity,intent,2(o165):
Activity,getIntent,3(o165):
Activity,onCreate,4(o165):
Activity,onClick,5(o165):
Activity,addButton,6(o165):
Activity,startActivity,7(o165):
Intent,Intent,1(o165):
Intent,target,2(o165):
Intent,data,3(o165):
Intent,setTarget,4(o165):
Intent,setData,5(o165):
Intent,getData,6(o165):
Main,Main,1(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,Game,1(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,Help,1(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,Score,1(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Activity,Activity,1(o163):
Activity,intent,2(o163):
Activity,getIntent,3(o163):
Activity,onCreate,4(o163):
Activity,onClick,5(o163):
Activity,addButton,6(o163):
Activity,startActivity,7(o163):
Intent,Intent,1(o163):
 - { o163 } <= x206
 - null{x207} <: Intent{x206}.target
 - null{x209} <: Intent{x206}.data
 - { o163 } <= x205
 - Object{x212}.Object <: ()--e213-->Object{x212}
 - { o163 } <= x212
 - e213 U {} <= e211
Intent,target,2(o163):
Intent,data,3(o163):
Intent,setTarget,4(o163):
 - { o163 } <= x206
 - String{x214} <: Intent{x206}.target
 - {  } <= x217
 - {} U {} <= e216
Intent,setData,5(o163):
 - { o163 } <= x206
 - Object{x218} <: Intent{x206}.data
 - {  } <= x221
 - {} U {} <= e220
Intent,getData,6(o163):
 - { o163 } <= x206
 - Intent{x206}.data <: Object{x224}
 - Object{x224} <: Object{x222}
 - {} <= e223
Main,Main,1(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,Game,1(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,Help,1(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,Score,1(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Activity,Activity,1(o154):
Activity,intent,2(o154):
Activity,getIntent,3(o154):
Activity,onCreate,4(o154):
Activity,onClick,5(o154):
Activity,addButton,6(o154):
Activity,startActivity,7(o154):
Intent,Intent,1(o154):
Intent,target,2(o154):
Intent,data,3(o154):
Intent,setTarget,4(o154):
Intent,setData,5(o154):
Intent,getData,6(o154):
Main,Main,1(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,Game,1(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,Help,1(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,Score,1(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Activity,Activity,1(o152):
Activity,intent,2(o152):
Activity,getIntent,3(o152):
Activity,onCreate,4(o152):
Activity,onClick,5(o152):
Activity,addButton,6(o152):
Activity,startActivity,7(o152):
Intent,Intent,1(o152):
 - { o152 } <= x226
 - null{x227} <: Intent{x226}.target
 - null{x229} <: Intent{x226}.data
 - { o152 } <= x225
 - Object{x232}.Object <: ()--e233-->Object{x232}
 - { o152 } <= x232
 - e233 U {} <= e231
Intent,target,2(o152):
Intent,data,3(o152):
Intent,setTarget,4(o152):
 - { o152 } <= x226
 - String{x234} <: Intent{x226}.target
 - {  } <= x237
 - {} U {} <= e236
Intent,setData,5(o152):
 - { o152 } <= x226
 - Object{x238} <: Intent{x226}.data
 - {  } <= x241
 - {} U {} <= e240
Intent,getData,6(o152):
 - { o152 } <= x226
 - Intent{x226}.data <: Object{x244}
 - Object{x244} <: Object{x242}
 - {} <= e243
Main,Main,1(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,Game,1(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,Help,1(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,Score,1(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Generating objects [1 iterations]

Initial constraints
 - {} <= e243
 - Object{x244} <: Object{x242}
 - Intent{x226}.data <: Object{x244}
 - { o152 } <= x226
 - {} U {} <= e240
 - {  } <= x241
 - Object{x238} <: Intent{x226}.data
 - {} U {} <= e236
 - {  } <= x237
 - String{x234} <: Intent{x226}.target
 - e233 U {} <= e231
 - { o152 } <= x232
 - Object{x232}.Object <: ()--e233-->Object{x232}
 - { o152 } <= x225
 - null{x229} <: Intent{x226}.data
 - null{x227} <: Intent{x226}.target
 - {} <= e223
 - Object{x224} <: Object{x222}
 - Intent{x206}.data <: Object{x224}
 - { o163 } <= x206
 - {} U {} <= e220
 - {  } <= x221
 - Object{x218} <: Intent{x206}.data
 - {} U {} <= e216
 - {  } <= x217
 - String{x214} <: Intent{x206}.target
 - e213 U {} <= e211
 - { o163 } <= x212
 - Object{x212}.Object <: ()--e213-->Object{x212}
 - { o163 } <= x205
 - null{x209} <: Intent{x206}.data
 - null{x207} <: Intent{x206}.target
 - {} <= e203
 - Object{x204} <: Object{x202}
 - Intent{x186}.data <: Object{x204}
 - { o174 } <= x186
 - {} U {} <= e200
 - {  } <= x201
 - Object{x198} <: Intent{x186}.data
 - {} U {} <= e196
 - {  } <= x197
 - String{x194} <: Intent{x186}.target
 - e193 U {} <= e191
 - { o174 } <= x192
 - Object{x192}.Object <: ()--e193-->Object{x192}
 - { o174 } <= x185
 - null{x189} <: Intent{x186}.data
 - null{x187} <: Intent{x186}.target
 - (int{x145})--e147-->void{x146} <: (int{x113})--e115-->void{x114}
 - ()--e132-->void{x131} <: ()--e112-->void{x111}
 - {} U {} U {} U {} U e151 U {} U {} U {} U {} U e156 U {} U {} U {} U e158 U {} U {} U {} U {} U e162 U {} U {} U {} U {} U e167 U {} U {} U {} U e169 U {} U {} U {} U {} U e173 U {} U {} U {} U {} U e178 U {} U {} U {} U e182 U {} U {} U {} U e184 U {} <= e147
 - Main{x127}.startActivity <: (Intent{x144})--e184-->void{x183}
 - Intent{x144}.setData <: (String{x179})--e182-->void{x181}
 - { o180 } <= x179
 - Intent{x144}.setTarget <: (String{x175})--e178-->void{x177}
 - { o176 } <= x175
 - Intent{x172} <: Intent{x144}
 - Intent{x172}.Intent <: ()--e173-->Intent{x172}
 - { o174 } <= x172
 - Main{x127}.startActivity <: (Intent{x143})--e169-->void{x168}
 - Intent{x143}.setTarget <: (String{x164})--e167-->void{x166}
 - { o165 } <= x164
 - Intent{x161} <: Intent{x143}
 - Intent{x161}.Intent <: ()--e162-->Intent{x161}
 - { o163 } <= x161
 - Main{x127}.startActivity <: (Intent{x142})--e158-->void{x157}
 - Intent{x142}.setTarget <: (String{x153})--e156-->void{x155}
 - { o154 } <= x153
 - Intent{x150} <: Intent{x142}
 - Intent{x150}.Intent <: ()--e151-->Intent{x150}
 - { o152 } <= x150
 - { o1 } <= x127
 - {} U {} U {} U e135 U {} U {} U {} U e138 U {} U {} U {} U e141 <= e132
 - Main{x127}.addButton <: (int{x139})--e141-->void{x140}
 - Main{x127}.addButton <: (int{x136})--e138-->void{x137}
 - Main{x127}.addButton <: (int{x133})--e135-->void{x134}
 - e130 U {} <= e128
 - { o1 } <= x129
 - Activity{x129}.Activity <: ()--e130-->Activity{x129}
 - { o1 } <= x126
 - {} U {} U e124 <= e123
 - Intent{x121} ~~~> e124 @ [{o1},ActivitystartActivity,7,1, n/a]
 - { o1 } <= x102
 - {} U {} U e119 <= e118
 - {} <= e115
 - {} <= e112
 - {} <= e109
 - Intent{x110} <: Intent{x108}
 - Activity{x102}.intent <: Intent{x110}
 - e107 U {} <= e105
 - { o1 } <= x106
 - Object{x106}.Object <: ()--e107-->Object{x106}
 - { o1 } <= x101
 - null{x103} <: Activity{x102}.intent

. . . 
Solving Constraints [3 iterations]
The solution is:
 - x101 = { o1 }
 - x102 = { o1 }
 - x104 = {  }
 - x106 = { o1 }
 - x108 = {  }
 - x110 = {  }
 - x111 = {  }
 - x114 = {  }
 - x116 = {  }
 - x121 = { o152,o163,o174 }
 - x126 = { o1 }
 - x127 = { o1 }
 - x129 = { o1 }
 - x134 = {  }
 - x137 = {  }
 - x140 = {  }
 - x142 = { o152 }
 - x143 = { o163 }
 - x144 = { o174 }
 - x145 = {  }
 - x150 = { o152 }
 - x153 = { o154 }
 - x155 = {  }
 - x157 = {  }
 - x161 = { o163 }
 - x164 = { o165 }
 - x166 = {  }
 - x168 = {  }
 - x172 = { o174 }
 - x175 = { o176 }
 - x177 = {  }
 - x179 = { o180 }
 - x181 = {  }
 - x183 = {  }
 - x185 = { o174 }
 - x186 = { o174 }
 - x188 = { o176 }
 - x190 = { o180 }
 - x192 = { o174 }
 - x194 = { o176 }
 - x197 = {  }
 - x198 = { o180 }
 - x201 = {  }
 - x202 = { o180 }
 - x204 = { o180 }
 - x205 = { o163 }
 - x206 = { o163 }
 - x208 = { o165 }
 - x210 = {  }
 - x212 = { o163 }
 - x214 = { o165 }
 - x217 = {  }
 - x221 = {  }
 - x222 = {  }
 - x224 = {  }
 - x225 = { o152 }
 - x226 = { o152 }
 - x228 = { o154 }
 - x230 = {  }
 - x232 = { o152 }
 - x234 = { o154 }
 - x237 = {  }
 - x241 = {  }
 - x242 = {  }
 - x244 = {  }
 - x246 = { o245 }
 - x248 = { o247 }
 - x250 = { o249 }

 - e105 = {}
 - e109 = {}
 - e112 = {}
 - e115 = {Game,Score,Help}
 - e118 = {}
 - e123 = {Game,Score,Help}
 - e124 = {Game,Score,Help}
 - e128 = {}
 - e130 = {}
 - e132 = {}
 - e135 = {}
 - e138 = {}
 - e141 = {}
 - e147 = {Game,Score,Help}
 - e151 = {}
 - e156 = {}
 - e158 = {Game,Score,Help}
 - e162 = {}
 - e167 = {}
 - e169 = {Game,Score,Help}
 - e173 = {}
 - e178 = {}
 - e182 = {}
 - e184 = {Game,Score,Help}
 - e191 = {}
 - e196 = {}
 - e200 = {}
 - e203 = {}
 - e211 = {}
 - e216 = {}
 - e220 = {}
 - e223 = {}
 - e231 = {}
 - e236 = {}
 - e240 = {}
 - e243 = {}


### Round 2###

Activity,Activity,1(o249):
 - { o249 } <= x252
 - null{x253} <: Activity{x252}.intent
 - { o249 } <= x251
 - Object{x256}.Object <: ()--e257-->Object{x256}
 - { o249 } <= x256
 - e257 U {} <= e255
Activity,intent,2(o249):
Activity,getIntent,3(o249):
 - { o249 } <= x252
 - Activity{x252}.intent <: Intent{x260}
 - Intent{x260} <: Intent{x258}
 - {} <= e259
Activity,onCreate,4(o249):
 - { o249 } <= x252
 - {} <= e262
Activity,onClick,5(o249):
 - { o249 } <= x252
 - {} <= e265
Activity,addButton,6(o249):
 - { o249 } <= x252
 - {} U {} U e269 <= e268
Activity,startActivity,7(o249):
 - { o249 } <= x252
 - Intent{x271} ~~~> e274 @ [{o249},ActivitystartActivity,7,1, n/a]
 - {} U {} U e274 <= e273
Intent,Intent,1(o249):
Intent,target,2(o249):
Intent,data,3(o249):
Intent,setTarget,4(o249):
Intent,setData,5(o249):
Intent,getData,6(o249):
Main,Main,1(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,Game,1(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,Help,1(o249):
 - { o249 } <= x277
 - { o249 } <= x276
 - Activity{x279}.Activity <: ()--e280-->Activity{x279}
 - { o249 } <= x279
 - e280 U {} <= e278
Help,onCreate,2(o249):
 - { o249 } <= x277
 - Help{x277}.addButton <: (int{x283})--e285-->void{x284}
 - {} U {} U {} U e285 <= e282
Help,onClick,3(o249):
 - { o249 } <= x277
 - { o295 } <= x293
 - Intent{x293}.Intent <: ()--e294-->Intent{x293}
 - Intent{x293} <: Intent{x286}
 - Help{x277}.getIntent <: ()--e297-->Intent{x296}
 - Intent{x296} <: Intent{x287}
 - Intent{x287}.getData <: ()--e299-->Object{x298}
 - Object{x298} <: Object{x288}
 - String{x288} <: String{x289}
 - Intent{x286}.setTarget <: (String{x289})--e301-->void{x300}
 - Help{x277}.startActivity <: (Intent{x286})--e303-->void{x302}
 - {} U e294 U {} U {} U {} U e297 U {} U {} U e299 U {} U {} U {} U {} U e301 U {} U {} U {} U e303 <= e292
Score,Score,1(o249):
Score,onCreate,2(o249):
Score,onClick,3(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,onCreate,2(o249):
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
Help,onClick,3(o249):
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
Score,onCreate,2(o249):
Score,onClick,3(o249):
Activity,Activity,1(o247):
 - { o247 } <= x305
 - null{x306} <: Activity{x305}.intent
 - { o247 } <= x304
 - Object{x309}.Object <: ()--e310-->Object{x309}
 - { o247 } <= x309
 - e310 U {} <= e308
Activity,intent,2(o247):
Activity,getIntent,3(o247):
 - { o247 } <= x305
 - Activity{x305}.intent <: Intent{x313}
 - Intent{x313} <: Intent{x311}
 - {} <= e312
Activity,onCreate,4(o247):
 - { o247 } <= x305
 - {} <= e315
Activity,onClick,5(o247):
 - { o247 } <= x305
 - {} <= e318
Activity,addButton,6(o247):
 - { o247 } <= x305
 - {} U {} U e322 <= e321
Activity,startActivity,7(o247):
 - { o247 } <= x305
 - Intent{x324} ~~~> e327 @ [{o247},ActivitystartActivity,7,1, n/a]
 - {} U {} U e327 <= e326
Intent,Intent,1(o247):
Intent,target,2(o247):
Intent,data,3(o247):
Intent,setTarget,4(o247):
Intent,setData,5(o247):
Intent,getData,6(o247):
Main,Main,1(o247):
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,Game,1(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,Help,1(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,Score,1(o247):
 - { o247 } <= x330
 - { o247 } <= x329
 - Activity{x332}.Activity <: ()--e333-->Activity{x332}
 - { o247 } <= x332
 - e333 U {} <= e331
Score,onCreate,2(o247):
 - { o247 } <= x330
 - Score{x330}.addButton <: (int{x336})--e338-->void{x337}
 - {} U {} U {} U e338 <= e335
Score,onClick,3(o247):
 - { o247 } <= x330
 - { o345 } <= x343
 - Intent{x343}.Intent <: ()--e344-->Intent{x343}
 - Intent{x343} <: Intent{x339}
 - { o347 } <= x346
 - Intent{x339}.setTarget <: (String{x346})--e349-->void{x348}
 - Score{x330}.startActivity <: (Intent{x339})--e351-->void{x350}
 - {} U e344 U {} U {} U {} U {} U e349 U {} U {} U {} U e351 <= e342
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,onCreate,2(o247):
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
Score,onClick,3(o247):
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
Activity,Activity,1(o245):
 - { o245 } <= x353
 - null{x354} <: Activity{x353}.intent
 - { o245 } <= x352
 - Object{x357}.Object <: ()--e358-->Object{x357}
 - { o245 } <= x357
 - e358 U {} <= e356
Activity,intent,2(o245):
Activity,getIntent,3(o245):
 - { o245 } <= x353
 - Activity{x353}.intent <: Intent{x361}
 - Intent{x361} <: Intent{x359}
 - {} <= e360
Activity,onCreate,4(o245):
 - { o245 } <= x353
 - {} <= e363
Activity,onClick,5(o245):
 - { o245 } <= x353
 - {} <= e366
Activity,addButton,6(o245):
 - { o245 } <= x353
 - {} U {} U e370 <= e369
Activity,startActivity,7(o245):
 - { o245 } <= x353
 - Intent{x372} ~~~> e375 @ [{o245},ActivitystartActivity,7,1, n/a]
 - {} U {} U e375 <= e374
Intent,Intent,1(o245):
Intent,target,2(o245):
Intent,data,3(o245):
Intent,setTarget,4(o245):
Intent,setData,5(o245):
Intent,getData,6(o245):
Main,Main,1(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,Game,1(o245):
 - { o245 } <= x378
 - { o245 } <= x377
 - Activity{x380}.Activity <: ()--e381-->Activity{x380}
 - { o245 } <= x380
 - e381 U {} <= e379
Game,onCreate,2(o245):
 - { o245 } <= x378
 - Game{x378}.addButton <: (int{x384})--e386-->void{x385}
 - Game{x378}.addButton <: (int{x387})--e389-->void{x388}
 - Game{x378}.addButton <: (int{x390})--e392-->void{x391}
 - {} U {} U {} U e386 U {} U {} U {} U e389 U {} U {} U {} U e392 <= e383
Game,onClick,3(o245):
 - { o245 } <= x378
 - { o402 } <= x400
 - Intent{x400}.Intent <: ()--e401-->Intent{x400}
 - Intent{x400} <: Intent{x393}
 - { o404 } <= x403
 - Intent{x393}.setTarget <: (String{x403})--e406-->void{x405}
 - { o408 } <= x407
 - Intent{x393}.setData <: (String{x407})--e410-->void{x409}
 - Game{x378}.startActivity <: (Intent{x393})--e412-->void{x411}
 - { o417 } <= x415
 - Intent{x415}.Intent <: ()--e416-->Intent{x415}
 - Intent{x415} <: Intent{x394}
 - { o419 } <= x418
 - Intent{x394}.setTarget <: (String{x418})--e421-->void{x420}
 - Game{x378}.startActivity <: (Intent{x394})--e423-->void{x422}
 - {} U {} U {} U {} U e401 U {} U {} U {} U {} U e406 U {} U {} U {} U e410 U {} U {} U {} U e412 U {} U {} U {} U {} U e416 U {} U {} U {} U {} U e421 U {} U {} U {} U e423 U {} U {} U {} U {} U {} <= e397
Help,Help,1(o245):
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,Score,1(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,onCreate,2(o245):
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
Game,onClick,3(o245):
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Activity,Activity,1(o180):
Activity,intent,2(o180):
Activity,getIntent,3(o180):
Activity,onCreate,4(o180):
Activity,onClick,5(o180):
Activity,addButton,6(o180):
Activity,startActivity,7(o180):
Intent,Intent,1(o180):
Intent,target,2(o180):
Intent,data,3(o180):
Intent,setTarget,4(o180):
Intent,setData,5(o180):
Intent,getData,6(o180):
Main,Main,1(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,Game,1(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,Help,1(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,Score,1(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Activity,Activity,1(o176):
Activity,intent,2(o176):
Activity,getIntent,3(o176):
Activity,onCreate,4(o176):
Activity,onClick,5(o176):
Activity,addButton,6(o176):
Activity,startActivity,7(o176):
Intent,Intent,1(o176):
Intent,target,2(o176):
Intent,data,3(o176):
Intent,setTarget,4(o176):
Intent,setData,5(o176):
Intent,getData,6(o176):
Main,Main,1(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,Game,1(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,Help,1(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,Score,1(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Activity,Activity,1(o174):
Activity,intent,2(o174):
Activity,getIntent,3(o174):
Activity,onCreate,4(o174):
Activity,onClick,5(o174):
Activity,addButton,6(o174):
Activity,startActivity,7(o174):
Intent,Intent,1(o174):
Intent,target,2(o174):
Intent,data,3(o174):
Intent,setTarget,4(o174):
Intent,setData,5(o174):
Intent,getData,6(o174):
Main,Main,1(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,Game,1(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,Help,1(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,Score,1(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Activity,Activity,1(o165):
Activity,intent,2(o165):
Activity,getIntent,3(o165):
Activity,onCreate,4(o165):
Activity,onClick,5(o165):
Activity,addButton,6(o165):
Activity,startActivity,7(o165):
Intent,Intent,1(o165):
Intent,target,2(o165):
Intent,data,3(o165):
Intent,setTarget,4(o165):
Intent,setData,5(o165):
Intent,getData,6(o165):
Main,Main,1(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,Game,1(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,Help,1(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,Score,1(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Activity,Activity,1(o163):
Activity,intent,2(o163):
Activity,getIntent,3(o163):
Activity,onCreate,4(o163):
Activity,onClick,5(o163):
Activity,addButton,6(o163):
Activity,startActivity,7(o163):
Intent,Intent,1(o163):
Intent,target,2(o163):
Intent,data,3(o163):
Intent,setTarget,4(o163):
Intent,setData,5(o163):
Intent,getData,6(o163):
Main,Main,1(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,Game,1(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,Help,1(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,Score,1(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Activity,Activity,1(o154):
Activity,intent,2(o154):
Activity,getIntent,3(o154):
Activity,onCreate,4(o154):
Activity,onClick,5(o154):
Activity,addButton,6(o154):
Activity,startActivity,7(o154):
Intent,Intent,1(o154):
Intent,target,2(o154):
Intent,data,3(o154):
Intent,setTarget,4(o154):
Intent,setData,5(o154):
Intent,getData,6(o154):
Main,Main,1(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,Game,1(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,Help,1(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,Score,1(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Activity,Activity,1(o152):
Activity,intent,2(o152):
Activity,getIntent,3(o152):
Activity,onCreate,4(o152):
Activity,onClick,5(o152):
Activity,addButton,6(o152):
Activity,startActivity,7(o152):
Intent,Intent,1(o152):
Intent,target,2(o152):
Intent,data,3(o152):
Intent,setTarget,4(o152):
Intent,setData,5(o152):
Intent,getData,6(o152):
Main,Main,1(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,Game,1(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,Help,1(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,Score,1(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Activity,Activity,1(o419):
Activity,intent,2(o419):
Activity,getIntent,3(o419):
Activity,onCreate,4(o419):
Activity,onClick,5(o419):
Activity,addButton,6(o419):
Activity,startActivity,7(o419):
Intent,Intent,1(o419):
Intent,target,2(o419):
Intent,data,3(o419):
Intent,setTarget,4(o419):
Intent,setData,5(o419):
Intent,getData,6(o419):
Main,Main,1(o419):
Main,onCreate,2(o419):
Main,onClick,3(o419):
Game,Game,1(o419):
Game,onCreate,2(o419):
Game,onClick,3(o419):
Help,Help,1(o419):
Help,onCreate,2(o419):
Help,onClick,3(o419):
Score,Score,1(o419):
Score,onCreate,2(o419):
Score,onClick,3(o419):
Main,onCreate,2(o419):
Main,onClick,3(o419):
Game,onCreate,2(o419):
Game,onClick,3(o419):
Help,onCreate,2(o419):
Help,onClick,3(o419):
Score,onCreate,2(o419):
Score,onClick,3(o419):
Activity,Activity,1(o417):
Activity,intent,2(o417):
Activity,getIntent,3(o417):
Activity,onCreate,4(o417):
Activity,onClick,5(o417):
Activity,addButton,6(o417):
Activity,startActivity,7(o417):
Intent,Intent,1(o417):
 - { o417 } <= x427
 - null{x428} <: Intent{x427}.target
 - null{x430} <: Intent{x427}.data
 - { o417 } <= x426
 - Object{x433}.Object <: ()--e434-->Object{x433}
 - { o417 } <= x433
 - e434 U {} <= e432
Intent,target,2(o417):
Intent,data,3(o417):
Intent,setTarget,4(o417):
 - { o417 } <= x427
 - String{x435} <: Intent{x427}.target
 - {  } <= x438
 - {} U {} <= e437
Intent,setData,5(o417):
 - { o417 } <= x427
 - Object{x439} <: Intent{x427}.data
 - {  } <= x442
 - {} U {} <= e441
Intent,getData,6(o417):
 - { o417 } <= x427
 - Intent{x427}.data <: Object{x445}
 - Object{x445} <: Object{x443}
 - {} <= e444
Main,Main,1(o417):
Main,onCreate,2(o417):
Main,onClick,3(o417):
Game,Game,1(o417):
Game,onCreate,2(o417):
Game,onClick,3(o417):
Help,Help,1(o417):
Help,onCreate,2(o417):
Help,onClick,3(o417):
Score,Score,1(o417):
Score,onCreate,2(o417):
Score,onClick,3(o417):
Main,onCreate,2(o417):
Main,onClick,3(o417):
Game,onCreate,2(o417):
Game,onClick,3(o417):
Help,onCreate,2(o417):
Help,onClick,3(o417):
Score,onCreate,2(o417):
Score,onClick,3(o417):
Activity,Activity,1(o408):
Activity,intent,2(o408):
Activity,getIntent,3(o408):
Activity,onCreate,4(o408):
Activity,onClick,5(o408):
Activity,addButton,6(o408):
Activity,startActivity,7(o408):
Intent,Intent,1(o408):
Intent,target,2(o408):
Intent,data,3(o408):
Intent,setTarget,4(o408):
Intent,setData,5(o408):
Intent,getData,6(o408):
Main,Main,1(o408):
Main,onCreate,2(o408):
Main,onClick,3(o408):
Game,Game,1(o408):
Game,onCreate,2(o408):
Game,onClick,3(o408):
Help,Help,1(o408):
Help,onCreate,2(o408):
Help,onClick,3(o408):
Score,Score,1(o408):
Score,onCreate,2(o408):
Score,onClick,3(o408):
Main,onCreate,2(o408):
Main,onClick,3(o408):
Game,onCreate,2(o408):
Game,onClick,3(o408):
Help,onCreate,2(o408):
Help,onClick,3(o408):
Score,onCreate,2(o408):
Score,onClick,3(o408):
Activity,Activity,1(o404):
Activity,intent,2(o404):
Activity,getIntent,3(o404):
Activity,onCreate,4(o404):
Activity,onClick,5(o404):
Activity,addButton,6(o404):
Activity,startActivity,7(o404):
Intent,Intent,1(o404):
Intent,target,2(o404):
Intent,data,3(o404):
Intent,setTarget,4(o404):
Intent,setData,5(o404):
Intent,getData,6(o404):
Main,Main,1(o404):
Main,onCreate,2(o404):
Main,onClick,3(o404):
Game,Game,1(o404):
Game,onCreate,2(o404):
Game,onClick,3(o404):
Help,Help,1(o404):
Help,onCreate,2(o404):
Help,onClick,3(o404):
Score,Score,1(o404):
Score,onCreate,2(o404):
Score,onClick,3(o404):
Main,onCreate,2(o404):
Main,onClick,3(o404):
Game,onCreate,2(o404):
Game,onClick,3(o404):
Help,onCreate,2(o404):
Help,onClick,3(o404):
Score,onCreate,2(o404):
Score,onClick,3(o404):
Activity,Activity,1(o402):
Activity,intent,2(o402):
Activity,getIntent,3(o402):
Activity,onCreate,4(o402):
Activity,onClick,5(o402):
Activity,addButton,6(o402):
Activity,startActivity,7(o402):
Intent,Intent,1(o402):
 - { o402 } <= x447
 - null{x448} <: Intent{x447}.target
 - null{x450} <: Intent{x447}.data
 - { o402 } <= x446
 - Object{x453}.Object <: ()--e454-->Object{x453}
 - { o402 } <= x453
 - e454 U {} <= e452
Intent,target,2(o402):
Intent,data,3(o402):
Intent,setTarget,4(o402):
 - { o402 } <= x447
 - String{x455} <: Intent{x447}.target
 - {  } <= x458
 - {} U {} <= e457
Intent,setData,5(o402):
 - { o402 } <= x447
 - Object{x459} <: Intent{x447}.data
 - {  } <= x462
 - {} U {} <= e461
Intent,getData,6(o402):
 - { o402 } <= x447
 - Intent{x447}.data <: Object{x465}
 - Object{x465} <: Object{x463}
 - {} <= e464
Main,Main,1(o402):
Main,onCreate,2(o402):
Main,onClick,3(o402):
Game,Game,1(o402):
Game,onCreate,2(o402):
Game,onClick,3(o402):
Help,Help,1(o402):
Help,onCreate,2(o402):
Help,onClick,3(o402):
Score,Score,1(o402):
Score,onCreate,2(o402):
Score,onClick,3(o402):
Main,onCreate,2(o402):
Main,onClick,3(o402):
Game,onCreate,2(o402):
Game,onClick,3(o402):
Help,onCreate,2(o402):
Help,onClick,3(o402):
Score,onCreate,2(o402):
Score,onClick,3(o402):
Activity,Activity,1(o347):
Activity,intent,2(o347):
Activity,getIntent,3(o347):
Activity,onCreate,4(o347):
Activity,onClick,5(o347):
Activity,addButton,6(o347):
Activity,startActivity,7(o347):
Intent,Intent,1(o347):
Intent,target,2(o347):
Intent,data,3(o347):
Intent,setTarget,4(o347):
Intent,setData,5(o347):
Intent,getData,6(o347):
Main,Main,1(o347):
Main,onCreate,2(o347):
Main,onClick,3(o347):
Game,Game,1(o347):
Game,onCreate,2(o347):
Game,onClick,3(o347):
Help,Help,1(o347):
Help,onCreate,2(o347):
Help,onClick,3(o347):
Score,Score,1(o347):
Score,onCreate,2(o347):
Score,onClick,3(o347):
Main,onCreate,2(o347):
Main,onClick,3(o347):
Game,onCreate,2(o347):
Game,onClick,3(o347):
Help,onCreate,2(o347):
Help,onClick,3(o347):
Score,onCreate,2(o347):
Score,onClick,3(o347):
Activity,Activity,1(o345):
Activity,intent,2(o345):
Activity,getIntent,3(o345):
Activity,onCreate,4(o345):
Activity,onClick,5(o345):
Activity,addButton,6(o345):
Activity,startActivity,7(o345):
Intent,Intent,1(o345):
 - { o345 } <= x467
 - null{x468} <: Intent{x467}.target
 - null{x470} <: Intent{x467}.data
 - { o345 } <= x466
 - Object{x473}.Object <: ()--e474-->Object{x473}
 - { o345 } <= x473
 - e474 U {} <= e472
Intent,target,2(o345):
Intent,data,3(o345):
Intent,setTarget,4(o345):
 - { o345 } <= x467
 - String{x475} <: Intent{x467}.target
 - {  } <= x478
 - {} U {} <= e477
Intent,setData,5(o345):
 - { o345 } <= x467
 - Object{x479} <: Intent{x467}.data
 - {  } <= x482
 - {} U {} <= e481
Intent,getData,6(o345):
 - { o345 } <= x467
 - Intent{x467}.data <: Object{x485}
 - Object{x485} <: Object{x483}
 - {} <= e484
Main,Main,1(o345):
Main,onCreate,2(o345):
Main,onClick,3(o345):
Game,Game,1(o345):
Game,onCreate,2(o345):
Game,onClick,3(o345):
Help,Help,1(o345):
Help,onCreate,2(o345):
Help,onClick,3(o345):
Score,Score,1(o345):
Score,onCreate,2(o345):
Score,onClick,3(o345):
Main,onCreate,2(o345):
Main,onClick,3(o345):
Game,onCreate,2(o345):
Game,onClick,3(o345):
Help,onCreate,2(o345):
Help,onClick,3(o345):
Score,onCreate,2(o345):
Score,onClick,3(o345):
Activity,Activity,1(o295):
Activity,intent,2(o295):
Activity,getIntent,3(o295):
Activity,onCreate,4(o295):
Activity,onClick,5(o295):
Activity,addButton,6(o295):
Activity,startActivity,7(o295):
Intent,Intent,1(o295):
 - { o295 } <= x487
 - null{x488} <: Intent{x487}.target
 - null{x490} <: Intent{x487}.data
 - { o295 } <= x486
 - Object{x493}.Object <: ()--e494-->Object{x493}
 - { o295 } <= x493
 - e494 U {} <= e492
Intent,target,2(o295):
Intent,data,3(o295):
Intent,setTarget,4(o295):
 - { o295 } <= x487
 - String{x495} <: Intent{x487}.target
 - {  } <= x498
 - {} U {} <= e497
Intent,setData,5(o295):
 - { o295 } <= x487
 - Object{x499} <: Intent{x487}.data
 - {  } <= x502
 - {} U {} <= e501
Intent,getData,6(o295):
 - { o295 } <= x487
 - Intent{x487}.data <: Object{x505}
 - Object{x505} <: Object{x503}
 - {} <= e504
Main,Main,1(o295):
Main,onCreate,2(o295):
Main,onClick,3(o295):
Game,Game,1(o295):
Game,onCreate,2(o295):
Game,onClick,3(o295):
Help,Help,1(o295):
Help,onCreate,2(o295):
Help,onClick,3(o295):
Score,Score,1(o295):
Score,onCreate,2(o295):
Score,onClick,3(o295):
Main,onCreate,2(o295):
Main,onClick,3(o295):
Game,onCreate,2(o295):
Game,onClick,3(o295):
Help,onCreate,2(o295):
Help,onClick,3(o295):
Score,onCreate,2(o295):
Score,onClick,3(o295):
Activity,Activity,1(o249):
Activity,intent,2(o249):
Activity,getIntent,3(o249):
Activity,onCreate,4(o249):
Activity,onClick,5(o249):
Activity,addButton,6(o249):
Activity,startActivity,7(o249):
Intent,Intent,1(o249):
Intent,target,2(o249):
Intent,data,3(o249):
Intent,setTarget,4(o249):
Intent,setData,5(o249):
Intent,getData,6(o249):
Main,Main,1(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,Game,1(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,Help,1(o249):
Help,onCreate,2(o249):
Help,onClick,3(o249):
Score,Score,1(o249):
Score,onCreate,2(o249):
Score,onClick,3(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,onCreate,2(o249):
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
Help,onClick,3(o249):
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
Score,onCreate,2(o249):
Score,onClick,3(o249):
Activity,Activity,1(o247):
Activity,intent,2(o247):
Activity,getIntent,3(o247):
Activity,onCreate,4(o247):
Activity,onClick,5(o247):
Activity,addButton,6(o247):
Activity,startActivity,7(o247):
Intent,Intent,1(o247):
Intent,target,2(o247):
Intent,data,3(o247):
Intent,setTarget,4(o247):
Intent,setData,5(o247):
Intent,getData,6(o247):
Main,Main,1(o247):
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,Game,1(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,Help,1(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,Score,1(o247):
Score,onCreate,2(o247):
Score,onClick,3(o247):
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,onCreate,2(o247):
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
Score,onClick,3(o247):
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
Activity,Activity,1(o245):
Activity,intent,2(o245):
Activity,getIntent,3(o245):
Activity,onCreate,4(o245):
Activity,onClick,5(o245):
Activity,addButton,6(o245):
Activity,startActivity,7(o245):
Intent,Intent,1(o245):
Intent,target,2(o245):
Intent,data,3(o245):
Intent,setTarget,4(o245):
Intent,setData,5(o245):
Intent,getData,6(o245):
Main,Main,1(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,Game,1(o245):
Game,onCreate,2(o245):
Game,onClick,3(o245):
Help,Help,1(o245):
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,Score,1(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,onCreate,2(o245):
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
Game,onClick,3(o245):
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Activity,Activity,1(o180):
Activity,intent,2(o180):
Activity,getIntent,3(o180):
Activity,onCreate,4(o180):
Activity,onClick,5(o180):
Activity,addButton,6(o180):
Activity,startActivity,7(o180):
Intent,Intent,1(o180):
Intent,target,2(o180):
Intent,data,3(o180):
Intent,setTarget,4(o180):
Intent,setData,5(o180):
Intent,getData,6(o180):
Main,Main,1(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,Game,1(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,Help,1(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,Score,1(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Activity,Activity,1(o176):
Activity,intent,2(o176):
Activity,getIntent,3(o176):
Activity,onCreate,4(o176):
Activity,onClick,5(o176):
Activity,addButton,6(o176):
Activity,startActivity,7(o176):
Intent,Intent,1(o176):
Intent,target,2(o176):
Intent,data,3(o176):
Intent,setTarget,4(o176):
Intent,setData,5(o176):
Intent,getData,6(o176):
Main,Main,1(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,Game,1(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,Help,1(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,Score,1(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Activity,Activity,1(o174):
Activity,intent,2(o174):
Activity,getIntent,3(o174):
Activity,onCreate,4(o174):
Activity,onClick,5(o174):
Activity,addButton,6(o174):
Activity,startActivity,7(o174):
Intent,Intent,1(o174):
Intent,target,2(o174):
Intent,data,3(o174):
Intent,setTarget,4(o174):
Intent,setData,5(o174):
Intent,getData,6(o174):
Main,Main,1(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,Game,1(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,Help,1(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,Score,1(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Activity,Activity,1(o165):
Activity,intent,2(o165):
Activity,getIntent,3(o165):
Activity,onCreate,4(o165):
Activity,onClick,5(o165):
Activity,addButton,6(o165):
Activity,startActivity,7(o165):
Intent,Intent,1(o165):
Intent,target,2(o165):
Intent,data,3(o165):
Intent,setTarget,4(o165):
Intent,setData,5(o165):
Intent,getData,6(o165):
Main,Main,1(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,Game,1(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,Help,1(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,Score,1(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Activity,Activity,1(o163):
Activity,intent,2(o163):
Activity,getIntent,3(o163):
Activity,onCreate,4(o163):
Activity,onClick,5(o163):
Activity,addButton,6(o163):
Activity,startActivity,7(o163):
Intent,Intent,1(o163):
Intent,target,2(o163):
Intent,data,3(o163):
Intent,setTarget,4(o163):
Intent,setData,5(o163):
Intent,getData,6(o163):
Main,Main,1(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,Game,1(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,Help,1(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,Score,1(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Activity,Activity,1(o154):
Activity,intent,2(o154):
Activity,getIntent,3(o154):
Activity,onCreate,4(o154):
Activity,onClick,5(o154):
Activity,addButton,6(o154):
Activity,startActivity,7(o154):
Intent,Intent,1(o154):
Intent,target,2(o154):
Intent,data,3(o154):
Intent,setTarget,4(o154):
Intent,setData,5(o154):
Intent,getData,6(o154):
Main,Main,1(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,Game,1(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,Help,1(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,Score,1(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Activity,Activity,1(o152):
Activity,intent,2(o152):
Activity,getIntent,3(o152):
Activity,onCreate,4(o152):
Activity,onClick,5(o152):
Activity,addButton,6(o152):
Activity,startActivity,7(o152):
Intent,Intent,1(o152):
Intent,target,2(o152):
Intent,data,3(o152):
Intent,setTarget,4(o152):
Intent,setData,5(o152):
Intent,getData,6(o152):
Main,Main,1(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,Game,1(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,Help,1(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,Score,1(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Generating objects [2 iterations]

Initial constraints
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
 - {} <= e504
 - Object{x505} <: Object{x503}
 - Intent{x487}.data <: Object{x505}
 - { o295 } <= x487
 - {} U {} <= e501
 - {  } <= x502
 - Object{x499} <: Intent{x487}.data
 - {} U {} <= e497
 - {  } <= x498
 - String{x495} <: Intent{x487}.target
 - e494 U {} <= e492
 - { o295 } <= x493
 - Object{x493}.Object <: ()--e494-->Object{x493}
 - { o295 } <= x486
 - null{x490} <: Intent{x487}.data
 - null{x488} <: Intent{x487}.target
 - {} <= e484
 - Object{x485} <: Object{x483}
 - Intent{x467}.data <: Object{x485}
 - { o345 } <= x467
 - {} U {} <= e481
 - {  } <= x482
 - Object{x479} <: Intent{x467}.data
 - {} U {} <= e477
 - {  } <= x478
 - String{x475} <: Intent{x467}.target
 - e474 U {} <= e472
 - { o345 } <= x473
 - Object{x473}.Object <: ()--e474-->Object{x473}
 - { o345 } <= x466
 - null{x470} <: Intent{x467}.data
 - null{x468} <: Intent{x467}.target
 - {} <= e464
 - Object{x465} <: Object{x463}
 - Intent{x447}.data <: Object{x465}
 - { o402 } <= x447
 - {} U {} <= e461
 - {  } <= x462
 - Object{x459} <: Intent{x447}.data
 - {} U {} <= e457
 - {  } <= x458
 - String{x455} <: Intent{x447}.target
 - e454 U {} <= e452
 - { o402 } <= x453
 - Object{x453}.Object <: ()--e454-->Object{x453}
 - { o402 } <= x446
 - null{x450} <: Intent{x447}.data
 - null{x448} <: Intent{x447}.target
 - {} <= e444
 - Object{x445} <: Object{x443}
 - Intent{x427}.data <: Object{x445}
 - { o417 } <= x427
 - {} U {} <= e441
 - {  } <= x442
 - Object{x439} <: Intent{x427}.data
 - {} U {} <= e437
 - {  } <= x438
 - String{x435} <: Intent{x427}.target
 - e434 U {} <= e432
 - { o417 } <= x433
 - Object{x433}.Object <: ()--e434-->Object{x433}
 - { o417 } <= x426
 - null{x430} <: Intent{x427}.data
 - null{x428} <: Intent{x427}.target
 - {} U {} U {} U {} U e401 U {} U {} U {} U {} U e406 U {} U {} U {} U e410 U {} U {} U {} U e412 U {} U {} U {} U {} U e416 U {} U {} U {} U {} U e421 U {} U {} U {} U e423 U {} U {} U {} U {} U {} <= e397
 - Game{x378}.startActivity <: (Intent{x394})--e423-->void{x422}
 - Intent{x394}.setTarget <: (String{x418})--e421-->void{x420}
 - { o419 } <= x418
 - Intent{x415} <: Intent{x394}
 - Intent{x415}.Intent <: ()--e416-->Intent{x415}
 - { o417 } <= x415
 - Game{x378}.startActivity <: (Intent{x393})--e412-->void{x411}
 - Intent{x393}.setData <: (String{x407})--e410-->void{x409}
 - { o408 } <= x407
 - Intent{x393}.setTarget <: (String{x403})--e406-->void{x405}
 - { o404 } <= x403
 - Intent{x400} <: Intent{x393}
 - Intent{x400}.Intent <: ()--e401-->Intent{x400}
 - { o402 } <= x400
 - { o245 } <= x378
 - {} U {} U {} U e386 U {} U {} U {} U e389 U {} U {} U {} U e392 <= e383
 - Game{x378}.addButton <: (int{x390})--e392-->void{x391}
 - Game{x378}.addButton <: (int{x387})--e389-->void{x388}
 - Game{x378}.addButton <: (int{x384})--e386-->void{x385}
 - e381 U {} <= e379
 - { o245 } <= x380
 - Activity{x380}.Activity <: ()--e381-->Activity{x380}
 - { o245 } <= x377
 - {} U {} U e375 <= e374
 - Intent{x372} ~~~> e375 @ [{o245},ActivitystartActivity,7,1, n/a]
 - { o245 } <= x353
 - {} U {} U e370 <= e369
 - {} <= e366
 - {} <= e363
 - {} <= e360
 - Intent{x361} <: Intent{x359}
 - Activity{x353}.intent <: Intent{x361}
 - e358 U {} <= e356
 - { o245 } <= x357
 - Object{x357}.Object <: ()--e358-->Object{x357}
 - { o245 } <= x352
 - null{x354} <: Activity{x353}.intent
 - {} U e344 U {} U {} U {} U {} U e349 U {} U {} U {} U e351 <= e342
 - Score{x330}.startActivity <: (Intent{x339})--e351-->void{x350}
 - Intent{x339}.setTarget <: (String{x346})--e349-->void{x348}
 - { o347 } <= x346
 - Intent{x343} <: Intent{x339}
 - Intent{x343}.Intent <: ()--e344-->Intent{x343}
 - { o345 } <= x343
 - { o247 } <= x330
 - {} U {} U {} U e338 <= e335
 - Score{x330}.addButton <: (int{x336})--e338-->void{x337}
 - e333 U {} <= e331
 - { o247 } <= x332
 - Activity{x332}.Activity <: ()--e333-->Activity{x332}
 - { o247 } <= x329
 - {} U {} U e327 <= e326
 - Intent{x324} ~~~> e327 @ [{o247},ActivitystartActivity,7,1, n/a]
 - { o247 } <= x305
 - {} U {} U e322 <= e321
 - {} <= e318
 - {} <= e315
 - {} <= e312
 - Intent{x313} <: Intent{x311}
 - Activity{x305}.intent <: Intent{x313}
 - e310 U {} <= e308
 - { o247 } <= x309
 - Object{x309}.Object <: ()--e310-->Object{x309}
 - { o247 } <= x304
 - null{x306} <: Activity{x305}.intent
 - {} U e294 U {} U {} U {} U e297 U {} U {} U e299 U {} U {} U {} U {} U e301 U {} U {} U {} U e303 <= e292
 - Help{x277}.startActivity <: (Intent{x286})--e303-->void{x302}
 - Intent{x286}.setTarget <: (String{x289})--e301-->void{x300}
 - String{x288} <: String{x289}
 - Object{x298} <: Object{x288}
 - Intent{x287}.getData <: ()--e299-->Object{x298}
 - Intent{x296} <: Intent{x287}
 - Help{x277}.getIntent <: ()--e297-->Intent{x296}
 - Intent{x293} <: Intent{x286}
 - Intent{x293}.Intent <: ()--e294-->Intent{x293}
 - { o295 } <= x293
 - { o249 } <= x277
 - {} U {} U {} U e285 <= e282
 - Help{x277}.addButton <: (int{x283})--e285-->void{x284}
 - e280 U {} <= e278
 - { o249 } <= x279
 - Activity{x279}.Activity <: ()--e280-->Activity{x279}
 - { o249 } <= x276
 - {} U {} U e274 <= e273
 - Intent{x271} ~~~> e274 @ [{o249},ActivitystartActivity,7,1, n/a]
 - { o249 } <= x252
 - {} U {} U e269 <= e268
 - {} <= e265
 - {} <= e262
 - {} <= e259
 - Intent{x260} <: Intent{x258}
 - Activity{x252}.intent <: Intent{x260}
 - e257 U {} <= e255
 - { o249 } <= x256
 - Object{x256}.Object <: ()--e257-->Object{x256}
 - { o249 } <= x251
 - null{x253} <: Activity{x252}.intent
 - {} <= e243
 - Object{x244} <: Object{x242}
 - Intent{x226}.data <: Object{x244}
 - Object{x230} <: Object{x244}
 - { o152 } <= x226
 - {} U {} <= e240
 - {  } <= x241
 - Object{x238} <: Intent{x226}.data
 - Object{x238} <: Object{x230}
 - {} U {} <= e236
 - {  } <= x237
 - String{x234} <: Intent{x226}.target
 - String{x234} <: String{x228}
 - e233 U {} <= e231
 - { o152 } <= x232
 - Object{x232}.Object <: ()--e233-->Object{x232}
 - { o152 } <= x225
 - null{x229} <: Intent{x226}.data
 - null{x229} <: Object{x230}
 - null{x227} <: Intent{x226}.target
 - null{x227} <: String{x228}
 - {} <= e223
 - Object{x224} <: Object{x222}
 - Intent{x206}.data <: Object{x224}
 - Object{x210} <: Object{x224}
 - { o163 } <= x206
 - {} U {} <= e220
 - {  } <= x221
 - Object{x218} <: Intent{x206}.data
 - Object{x218} <: Object{x210}
 - {} U {} <= e216
 - {  } <= x217
 - String{x214} <: Intent{x206}.target
 - String{x214} <: String{x208}
 - e213 U {} <= e211
 - { o163 } <= x212
 - Object{x212}.Object <: ()--e213-->Object{x212}
 - { o163 } <= x205
 - null{x209} <: Intent{x206}.data
 - null{x209} <: Object{x210}
 - null{x207} <: Intent{x206}.target
 - null{x207} <: String{x208}
 - {} <= e203
 - Object{x204} <: Object{x202}
 - Intent{x186}.data <: Object{x204}
 - Object{x190} <: Object{x204}
 - { o174 } <= x186
 - {} U {} <= e200
 - {  } <= x201
 - Object{x198} <: Intent{x186}.data
 - Object{x198} <: Object{x190}
 - {} U {} <= e196
 - {  } <= x197
 - String{x194} <: Intent{x186}.target
 - String{x194} <: String{x188}
 - e193 U {} <= e191
 - { o174 } <= x192
 - Object{x192}.Object <: ()--e193-->Object{x192}
 - { o174 } <= x185
 - null{x189} <: Intent{x186}.data
 - null{x189} <: Object{x190}
 - null{x187} <: Intent{x186}.target
 - null{x187} <: String{x188}
 - (int{x145})--e147-->void{x146} <: (int{x113})--e115-->void{x114}
 - ()--e132-->void{x131} <: ()--e112-->void{x111}
 - {} U {} U {} U {} U e151 U {} U {} U {} U {} U e156 U {} U {} U {} U e158 U {} U {} U {} U {} U e162 U {} U {} U {} U {} U e167 U {} U {} U {} U e169 U {} U {} U {} U {} U e173 U {} U {} U {} U {} U e178 U {} U {} U {} U e182 U {} U {} U {} U e184 U {} <= e147
 - Main{x127}.startActivity <: (Intent{x144})--e184-->void{x183}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x144})--e184-->void{x183}
 - Intent{x144}.setData <: (String{x179})--e182-->void{x181}
 - (Object{x198})--e200-->void{x199} <: (String{x179})--e182-->void{x181}
 - { o180 } <= x179
 - Intent{x144}.setTarget <: (String{x175})--e178-->void{x177}
 - (String{x194})--e196-->void{x195} <: (String{x175})--e178-->void{x177}
 - { o176 } <= x175
 - Intent{x172} <: Intent{x144}
 - Intent{x172}.Intent <: ()--e173-->Intent{x172}
 - ()--e191-->Intent{x185} <: ()--e173-->Intent{x172}
 - { o174 } <= x172
 - Main{x127}.startActivity <: (Intent{x143})--e169-->void{x168}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x143})--e169-->void{x168}
 - Intent{x143}.setTarget <: (String{x164})--e167-->void{x166}
 - (String{x214})--e216-->void{x215} <: (String{x164})--e167-->void{x166}
 - { o165 } <= x164
 - Intent{x161} <: Intent{x143}
 - Intent{x161}.Intent <: ()--e162-->Intent{x161}
 - ()--e211-->Intent{x205} <: ()--e162-->Intent{x161}
 - { o163 } <= x161
 - Main{x127}.startActivity <: (Intent{x142})--e158-->void{x157}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x142})--e158-->void{x157}
 - Intent{x142}.setTarget <: (String{x153})--e156-->void{x155}
 - (String{x234})--e236-->void{x235} <: (String{x153})--e156-->void{x155}
 - { o154 } <= x153
 - Intent{x150} <: Intent{x142}
 - Intent{x150}.Intent <: ()--e151-->Intent{x150}
 - ()--e231-->Intent{x225} <: ()--e151-->Intent{x150}
 - { o152 } <= x150
 - { o1 } <= x127
 - {} U {} U {} U e135 U {} U {} U {} U e138 U {} U {} U {} U e141 <= e132
 - Main{x127}.addButton <: (int{x139})--e141-->void{x140}
 - (int{x116})--e118-->void{x117} <: (int{x139})--e141-->void{x140}
 - Main{x127}.addButton <: (int{x136})--e138-->void{x137}
 - (int{x116})--e118-->void{x117} <: (int{x136})--e138-->void{x137}
 - Main{x127}.addButton <: (int{x133})--e135-->void{x134}
 - (int{x116})--e118-->void{x117} <: (int{x133})--e135-->void{x134}
 - e130 U {} <= e128
 - { o1 } <= x129
 - Activity{x129}.Activity <: ()--e130-->Activity{x129}
 - ()--e105-->Activity{x101} <: ()--e130-->Activity{x129}
 - { o1 } <= x126
 - {} U {} U e124 <= e123
 - Intent{x121} ~~~> e124 @ [{o1},ActivitystartActivity,7,1, n/a]
 - { o245 } <= x246
 - Intent{x121} <: Game{x246}.intent
 - { o247 } <= x248
 - Intent{x121} <: Score{x248}.intent
 - { o249 } <= x250
 - Intent{x121} <: Help{x250}.intent
 - {Game,Score,Help} <= e124
 - {} <= e124
 - { o1 } <= x102
 - {} U {} U e119 <= e118
 - {} <= e115
 - {} <= e112
 - {} <= e109
 - Intent{x110} <: Intent{x108}
 - Activity{x102}.intent <: Intent{x110}
 - Intent{x104} <: Intent{x110}
 - e107 U {} <= e105
 - { o1 } <= x106
 - Object{x106}.Object <: ()--e107-->Object{x106}
 - { o1 } <= x101
 - null{x103} <: Activity{x102}.intent
 - null{x103} <: Intent{x104}

. . . . . . . 
Solving Constraints [7 iterations]
The solution is:
 - x101 = { o1 }
 - x102 = { o1 }
 - x104 = {  }
 - x106 = { o1 }
 - x108 = {  }
 - x110 = {  }
 - x111 = {  }
 - x114 = {  }
 - x116 = {  }
 - x121 = { o152,o163,o174 }
 - x126 = { o1 }
 - x127 = { o1 }
 - x129 = { o1 }
 - x134 = {  }
 - x137 = {  }
 - x140 = {  }
 - x142 = { o152 }
 - x143 = { o163 }
 - x144 = { o174 }
 - x145 = {  }
 - x150 = { o152 }
 - x153 = { o154 }
 - x155 = {  }
 - x157 = {  }
 - x161 = { o163 }
 - x164 = { o165 }
 - x166 = {  }
 - x168 = {  }
 - x172 = { o174 }
 - x175 = { o176 }
 - x177 = {  }
 - x179 = { o180 }
 - x181 = {  }
 - x183 = {  }
 - x185 = { o174 }
 - x186 = { o174 }
 - x188 = { o176 }
 - x190 = { o180 }
 - x192 = { o174 }
 - x194 = { o176 }
 - x197 = {  }
 - x198 = { o180 }
 - x201 = {  }
 - x202 = { o180 }
 - x204 = { o180 }
 - x205 = { o163 }
 - x206 = { o163 }
 - x208 = { o165 }
 - x210 = {  }
 - x212 = { o163 }
 - x214 = { o165 }
 - x217 = {  }
 - x221 = {  }
 - x222 = {  }
 - x224 = {  }
 - x225 = { o152 }
 - x226 = { o152 }
 - x228 = { o154 }
 - x230 = {  }
 - x232 = { o152 }
 - x234 = { o154 }
 - x237 = {  }
 - x241 = {  }
 - x242 = {  }
 - x244 = {  }
 - x246 = { o245 }
 - x248 = { o247 }
 - x250 = { o249 }
 - x251 = { o249 }
 - x252 = { o249 }
 - x254 = { o152,o163,o174,o402,o417 }
 - x256 = { o249 }
 - x258 = { o152,o163,o174,o402,o417 }
 - x260 = { o152,o163,o174,o402,o417 }
 - x261 = {  }
 - x264 = {  }
 - x266 = {  }
 - x271 = { o295 }
 - x276 = { o249 }
 - x277 = { o249 }
 - x279 = { o249 }
 - x284 = {  }
 - x286 = { o295 }
 - x287 = { o152,o163,o174,o402,o417 }
 - x288 = { o180,o408 }
 - x289 = { o180,o408 }
 - x290 = {  }
 - x293 = { o295 }
 - x296 = { o152,o163,o174,o402,o417 }
 - x298 = { o180,o408 }
 - x300 = {  }
 - x302 = {  }
 - x304 = { o247 }
 - x305 = { o247 }
 - x307 = { o152,o163,o174,o402,o417 }
 - x309 = { o247 }
 - x311 = { o152,o163,o174,o402,o417 }
 - x313 = { o152,o163,o174,o402,o417 }
 - x314 = {  }
 - x317 = {  }
 - x319 = {  }
 - x324 = { o345 }
 - x329 = { o247 }
 - x330 = { o247 }
 - x332 = { o247 }
 - x337 = {  }
 - x339 = { o345 }
 - x340 = {  }
 - x343 = { o345 }
 - x346 = { o347 }
 - x348 = {  }
 - x350 = {  }
 - x352 = { o245 }
 - x353 = { o245 }
 - x355 = { o152,o163,o174,o295 }
 - x357 = { o245 }
 - x359 = { o152,o163,o174,o295 }
 - x361 = { o152,o163,o174,o295 }
 - x362 = {  }
 - x365 = {  }
 - x367 = {  }
 - x372 = { o402,o417 }
 - x377 = { o245 }
 - x378 = { o245 }
 - x380 = { o245 }
 - x385 = {  }
 - x388 = {  }
 - x391 = {  }
 - x393 = { o402 }
 - x394 = { o417 }
 - x395 = {  }
 - x400 = { o402 }
 - x403 = { o404 }
 - x405 = {  }
 - x407 = { o408 }
 - x409 = {  }
 - x411 = {  }
 - x415 = { o417 }
 - x418 = { o419 }
 - x420 = {  }
 - x422 = {  }
 - x426 = { o417 }
 - x427 = { o417 }
 - x429 = { o419 }
 - x431 = {  }
 - x433 = { o417 }
 - x435 = { o419 }
 - x438 = {  }
 - x442 = {  }
 - x443 = {  }
 - x445 = {  }
 - x446 = { o402 }
 - x447 = { o402 }
 - x449 = { o404 }
 - x451 = { o408 }
 - x453 = { o402 }
 - x455 = { o404 }
 - x458 = {  }
 - x459 = { o408 }
 - x462 = {  }
 - x463 = { o408 }
 - x465 = { o408 }
 - x466 = { o345 }
 - x467 = { o345 }
 - x469 = { o347 }
 - x471 = {  }
 - x473 = { o345 }
 - x475 = { o347 }
 - x478 = {  }
 - x482 = {  }
 - x483 = {  }
 - x485 = {  }
 - x486 = { o295 }
 - x487 = { o295 }
 - x489 = { o180,o408 }
 - x491 = {  }
 - x493 = { o295 }
 - x495 = { o180,o408 }
 - x498 = {  }
 - x502 = {  }
 - x503 = {  }
 - x505 = {  }
 - x507 = { o506 }

 - e105 = {}
 - e109 = {}
 - e112 = {}
 - e115 = {Game,Score,Help}
 - e118 = {}
 - e123 = {Game,Score,Help}
 - e124 = {Game,Score,Help}
 - e128 = {}
 - e130 = {}
 - e132 = {}
 - e135 = {}
 - e138 = {}
 - e141 = {}
 - e147 = {Game,Score,Help}
 - e151 = {}
 - e156 = {}
 - e158 = {Game,Score,Help}
 - e162 = {}
 - e167 = {}
 - e169 = {Game,Score,Help}
 - e173 = {}
 - e178 = {}
 - e182 = {}
 - e184 = {Game,Score,Help}
 - e191 = {}
 - e196 = {}
 - e200 = {}
 - e203 = {}
 - e211 = {}
 - e216 = {}
 - e220 = {}
 - e223 = {}
 - e231 = {}
 - e236 = {}
 - e240 = {}
 - e243 = {}
 - e255 = {}
 - e259 = {}
 - e262 = {}
 - e265 = {Main,Game}
 - e268 = {}
 - e273 = {Main,Game}
 - e274 = {Main,Game}
 - e278 = {}
 - e280 = {}
 - e282 = {}
 - e285 = {}
 - e292 = {Main,Game}
 - e294 = {}
 - e297 = {}
 - e299 = {}
 - e301 = {}
 - e303 = {Main,Game}
 - e308 = {}
 - e312 = {}
 - e315 = {}
 - e318 = {Main}
 - e321 = {}
 - e326 = {Main}
 - e327 = {Main}
 - e331 = {}
 - e333 = {}
 - e335 = {}
 - e338 = {}
 - e342 = {Main}
 - e344 = {}
 - e349 = {}
 - e351 = {Main}
 - e356 = {}
 - e360 = {}
 - e363 = {}
 - e366 = {Help,Score}
 - e369 = {}
 - e374 = {Help,Score}
 - e375 = {Help,Score}
 - e379 = {}
 - e381 = {}
 - e383 = {}
 - e386 = {}
 - e389 = {}
 - e392 = {}
 - e397 = {Help,Score}
 - e401 = {}
 - e406 = {}
 - e410 = {}
 - e412 = {Help,Score}
 - e416 = {}
 - e421 = {}
 - e423 = {Help,Score}
 - e432 = {}
 - e437 = {}
 - e441 = {}
 - e444 = {}
 - e452 = {}
 - e457 = {}
 - e461 = {}
 - e464 = {}
 - e472 = {}
 - e477 = {}
 - e481 = {}
 - e484 = {}
 - e492 = {}
 - e497 = {}
 - e501 = {}
 - e504 = {}


### Round 3###

Activity,Activity,1(o506):
 - { o506 } <= x509
 - null{x510} <: Activity{x509}.intent
 - { o506 } <= x508
 - Object{x513}.Object <: ()--e514-->Object{x513}
 - { o506 } <= x513
 - e514 U {} <= e512
Activity,intent,2(o506):
Activity,getIntent,3(o506):
 - { o506 } <= x509
 - Activity{x509}.intent <: Intent{x517}
 - Intent{x517} <: Intent{x515}
 - {} <= e516
Activity,onCreate,4(o506):
 - { o506 } <= x509
 - {} <= e519
Activity,onClick,5(o506):
 - { o506 } <= x509
 - {} <= e522
Activity,addButton,6(o506):
 - { o506 } <= x509
 - {} U {} U e526 <= e525
Activity,startActivity,7(o506):
 - { o506 } <= x509
 - Intent{x528} ~~~> e531 @ [{o506},ActivitystartActivity,7,1, n/a]
 - {} U {} U e531 <= e530
Intent,Intent,1(o506):
Intent,target,2(o506):
Intent,data,3(o506):
Intent,setTarget,4(o506):
Intent,setData,5(o506):
Intent,getData,6(o506):
Main,Main,1(o506):
 - { o506 } <= x534
 - { o506 } <= x533
 - Activity{x536}.Activity <: ()--e537-->Activity{x536}
 - { o506 } <= x536
 - e537 U {} <= e535
Main,onCreate,2(o506):
 - { o506 } <= x534
 - Main{x534}.addButton <: (int{x540})--e542-->void{x541}
 - Main{x534}.addButton <: (int{x543})--e545-->void{x544}
 - Main{x534}.addButton <: (int{x546})--e548-->void{x547}
 - {} U {} U {} U e542 U {} U {} U {} U e545 U {} U {} U {} U e548 <= e539
Main,onClick,3(o506):
 - { o506 } <= x534
 - { o152 } <= x557
 - Intent{x557}.Intent <: ()--e558-->Intent{x557}
 - Intent{x557} <: Intent{x549}
 - { o154 } <= x559
 - Intent{x549}.setTarget <: (String{x559})--e561-->void{x560}
 - Main{x534}.startActivity <: (Intent{x549})--e563-->void{x562}
 - { o163 } <= x566
 - Intent{x566}.Intent <: ()--e567-->Intent{x566}
 - Intent{x566} <: Intent{x550}
 - { o165 } <= x568
 - Intent{x550}.setTarget <: (String{x568})--e570-->void{x569}
 - Main{x534}.startActivity <: (Intent{x550})--e572-->void{x571}
 - { o174 } <= x575
 - Intent{x575}.Intent <: ()--e576-->Intent{x575}
 - Intent{x575} <: Intent{x551}
 - { o176 } <= x577
 - Intent{x551}.setTarget <: (String{x577})--e579-->void{x578}
 - { o180 } <= x580
 - Intent{x551}.setData <: (String{x580})--e582-->void{x581}
 - Main{x534}.startActivity <: (Intent{x551})--e584-->void{x583}
 - {} U {} U {} U {} U e558 U {} U {} U {} U {} U e561 U {} U {} U {} U e563 U {} U {} U {} U {} U e567 U {} U {} U {} U {} U e570 U {} U {} U {} U e572 U {} U {} U {} U {} U e576 U {} U {} U {} U {} U e579 U {} U {} U {} U e582 U {} U {} U {} U e584 U {} <= e554
Game,Game,1(o506):
Game,onCreate,2(o506):
Game,onClick,3(o506):
Help,Help,1(o506):
Help,onCreate,2(o506):
Help,onClick,3(o506):
Score,Score,1(o506):
Score,onCreate,2(o506):
Score,onClick,3(o506):
Main,onCreate,2(o506):
 - ()--e539-->void{x538} <: ()--e519-->void{x518}
Main,onClick,3(o506):
 - (int{x552})--e554-->void{x553} <: (int{x520})--e522-->void{x521}
Game,onCreate,2(o506):
Game,onClick,3(o506):
Help,onCreate,2(o506):
Help,onClick,3(o506):
Score,onCreate,2(o506):
Score,onClick,3(o506):
Activity,Activity,1(o419):
Activity,intent,2(o419):
Activity,getIntent,3(o419):
Activity,onCreate,4(o419):
Activity,onClick,5(o419):
Activity,addButton,6(o419):
Activity,startActivity,7(o419):
Intent,Intent,1(o419):
Intent,target,2(o419):
Intent,data,3(o419):
Intent,setTarget,4(o419):
Intent,setData,5(o419):
Intent,getData,6(o419):
Main,Main,1(o419):
Main,onCreate,2(o419):
Main,onClick,3(o419):
Game,Game,1(o419):
Game,onCreate,2(o419):
Game,onClick,3(o419):
Help,Help,1(o419):
Help,onCreate,2(o419):
Help,onClick,3(o419):
Score,Score,1(o419):
Score,onCreate,2(o419):
Score,onClick,3(o419):
Main,onCreate,2(o419):
Main,onClick,3(o419):
Game,onCreate,2(o419):
Game,onClick,3(o419):
Help,onCreate,2(o419):
Help,onClick,3(o419):
Score,onCreate,2(o419):
Score,onClick,3(o419):
Activity,Activity,1(o417):
Activity,intent,2(o417):
Activity,getIntent,3(o417):
Activity,onCreate,4(o417):
Activity,onClick,5(o417):
Activity,addButton,6(o417):
Activity,startActivity,7(o417):
Intent,Intent,1(o417):
Intent,target,2(o417):
Intent,data,3(o417):
Intent,setTarget,4(o417):
Intent,setData,5(o417):
Intent,getData,6(o417):
Main,Main,1(o417):
Main,onCreate,2(o417):
Main,onClick,3(o417):
Game,Game,1(o417):
Game,onCreate,2(o417):
Game,onClick,3(o417):
Help,Help,1(o417):
Help,onCreate,2(o417):
Help,onClick,3(o417):
Score,Score,1(o417):
Score,onCreate,2(o417):
Score,onClick,3(o417):
Main,onCreate,2(o417):
Main,onClick,3(o417):
Game,onCreate,2(o417):
Game,onClick,3(o417):
Help,onCreate,2(o417):
Help,onClick,3(o417):
Score,onCreate,2(o417):
Score,onClick,3(o417):
Activity,Activity,1(o408):
Activity,intent,2(o408):
Activity,getIntent,3(o408):
Activity,onCreate,4(o408):
Activity,onClick,5(o408):
Activity,addButton,6(o408):
Activity,startActivity,7(o408):
Intent,Intent,1(o408):
Intent,target,2(o408):
Intent,data,3(o408):
Intent,setTarget,4(o408):
Intent,setData,5(o408):
Intent,getData,6(o408):
Main,Main,1(o408):
Main,onCreate,2(o408):
Main,onClick,3(o408):
Game,Game,1(o408):
Game,onCreate,2(o408):
Game,onClick,3(o408):
Help,Help,1(o408):
Help,onCreate,2(o408):
Help,onClick,3(o408):
Score,Score,1(o408):
Score,onCreate,2(o408):
Score,onClick,3(o408):
Main,onCreate,2(o408):
Main,onClick,3(o408):
Game,onCreate,2(o408):
Game,onClick,3(o408):
Help,onCreate,2(o408):
Help,onClick,3(o408):
Score,onCreate,2(o408):
Score,onClick,3(o408):
Activity,Activity,1(o404):
Activity,intent,2(o404):
Activity,getIntent,3(o404):
Activity,onCreate,4(o404):
Activity,onClick,5(o404):
Activity,addButton,6(o404):
Activity,startActivity,7(o404):
Intent,Intent,1(o404):
Intent,target,2(o404):
Intent,data,3(o404):
Intent,setTarget,4(o404):
Intent,setData,5(o404):
Intent,getData,6(o404):
Main,Main,1(o404):
Main,onCreate,2(o404):
Main,onClick,3(o404):
Game,Game,1(o404):
Game,onCreate,2(o404):
Game,onClick,3(o404):
Help,Help,1(o404):
Help,onCreate,2(o404):
Help,onClick,3(o404):
Score,Score,1(o404):
Score,onCreate,2(o404):
Score,onClick,3(o404):
Main,onCreate,2(o404):
Main,onClick,3(o404):
Game,onCreate,2(o404):
Game,onClick,3(o404):
Help,onCreate,2(o404):
Help,onClick,3(o404):
Score,onCreate,2(o404):
Score,onClick,3(o404):
Activity,Activity,1(o402):
Activity,intent,2(o402):
Activity,getIntent,3(o402):
Activity,onCreate,4(o402):
Activity,onClick,5(o402):
Activity,addButton,6(o402):
Activity,startActivity,7(o402):
Intent,Intent,1(o402):
Intent,target,2(o402):
Intent,data,3(o402):
Intent,setTarget,4(o402):
Intent,setData,5(o402):
Intent,getData,6(o402):
Main,Main,1(o402):
Main,onCreate,2(o402):
Main,onClick,3(o402):
Game,Game,1(o402):
Game,onCreate,2(o402):
Game,onClick,3(o402):
Help,Help,1(o402):
Help,onCreate,2(o402):
Help,onClick,3(o402):
Score,Score,1(o402):
Score,onCreate,2(o402):
Score,onClick,3(o402):
Main,onCreate,2(o402):
Main,onClick,3(o402):
Game,onCreate,2(o402):
Game,onClick,3(o402):
Help,onCreate,2(o402):
Help,onClick,3(o402):
Score,onCreate,2(o402):
Score,onClick,3(o402):
Activity,Activity,1(o347):
Activity,intent,2(o347):
Activity,getIntent,3(o347):
Activity,onCreate,4(o347):
Activity,onClick,5(o347):
Activity,addButton,6(o347):
Activity,startActivity,7(o347):
Intent,Intent,1(o347):
Intent,target,2(o347):
Intent,data,3(o347):
Intent,setTarget,4(o347):
Intent,setData,5(o347):
Intent,getData,6(o347):
Main,Main,1(o347):
Main,onCreate,2(o347):
Main,onClick,3(o347):
Game,Game,1(o347):
Game,onCreate,2(o347):
Game,onClick,3(o347):
Help,Help,1(o347):
Help,onCreate,2(o347):
Help,onClick,3(o347):
Score,Score,1(o347):
Score,onCreate,2(o347):
Score,onClick,3(o347):
Main,onCreate,2(o347):
Main,onClick,3(o347):
Game,onCreate,2(o347):
Game,onClick,3(o347):
Help,onCreate,2(o347):
Help,onClick,3(o347):
Score,onCreate,2(o347):
Score,onClick,3(o347):
Activity,Activity,1(o345):
Activity,intent,2(o345):
Activity,getIntent,3(o345):
Activity,onCreate,4(o345):
Activity,onClick,5(o345):
Activity,addButton,6(o345):
Activity,startActivity,7(o345):
Intent,Intent,1(o345):
Intent,target,2(o345):
Intent,data,3(o345):
Intent,setTarget,4(o345):
Intent,setData,5(o345):
Intent,getData,6(o345):
Main,Main,1(o345):
Main,onCreate,2(o345):
Main,onClick,3(o345):
Game,Game,1(o345):
Game,onCreate,2(o345):
Game,onClick,3(o345):
Help,Help,1(o345):
Help,onCreate,2(o345):
Help,onClick,3(o345):
Score,Score,1(o345):
Score,onCreate,2(o345):
Score,onClick,3(o345):
Main,onCreate,2(o345):
Main,onClick,3(o345):
Game,onCreate,2(o345):
Game,onClick,3(o345):
Help,onCreate,2(o345):
Help,onClick,3(o345):
Score,onCreate,2(o345):
Score,onClick,3(o345):
Activity,Activity,1(o295):
Activity,intent,2(o295):
Activity,getIntent,3(o295):
Activity,onCreate,4(o295):
Activity,onClick,5(o295):
Activity,addButton,6(o295):
Activity,startActivity,7(o295):
Intent,Intent,1(o295):
Intent,target,2(o295):
Intent,data,3(o295):
Intent,setTarget,4(o295):
Intent,setData,5(o295):
Intent,getData,6(o295):
Main,Main,1(o295):
Main,onCreate,2(o295):
Main,onClick,3(o295):
Game,Game,1(o295):
Game,onCreate,2(o295):
Game,onClick,3(o295):
Help,Help,1(o295):
Help,onCreate,2(o295):
Help,onClick,3(o295):
Score,Score,1(o295):
Score,onCreate,2(o295):
Score,onClick,3(o295):
Main,onCreate,2(o295):
Main,onClick,3(o295):
Game,onCreate,2(o295):
Game,onClick,3(o295):
Help,onCreate,2(o295):
Help,onClick,3(o295):
Score,onCreate,2(o295):
Score,onClick,3(o295):
Activity,Activity,1(o249):
Activity,intent,2(o249):
Activity,getIntent,3(o249):
Activity,onCreate,4(o249):
Activity,onClick,5(o249):
Activity,addButton,6(o249):
Activity,startActivity,7(o249):
Intent,Intent,1(o249):
Intent,target,2(o249):
Intent,data,3(o249):
Intent,setTarget,4(o249):
Intent,setData,5(o249):
Intent,getData,6(o249):
Main,Main,1(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,Game,1(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,Help,1(o249):
Help,onCreate,2(o249):
Help,onClick,3(o249):
Score,Score,1(o249):
Score,onCreate,2(o249):
Score,onClick,3(o249):
Main,onCreate,2(o249):
Main,onClick,3(o249):
Game,onCreate,2(o249):
Game,onClick,3(o249):
Help,onCreate,2(o249):
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
Help,onClick,3(o249):
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
Score,onCreate,2(o249):
Score,onClick,3(o249):
Activity,Activity,1(o247):
Activity,intent,2(o247):
Activity,getIntent,3(o247):
Activity,onCreate,4(o247):
Activity,onClick,5(o247):
Activity,addButton,6(o247):
Activity,startActivity,7(o247):
Intent,Intent,1(o247):
Intent,target,2(o247):
Intent,data,3(o247):
Intent,setTarget,4(o247):
Intent,setData,5(o247):
Intent,getData,6(o247):
Main,Main,1(o247):
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,Game,1(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,Help,1(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,Score,1(o247):
Score,onCreate,2(o247):
Score,onClick,3(o247):
Main,onCreate,2(o247):
Main,onClick,3(o247):
Game,onCreate,2(o247):
Game,onClick,3(o247):
Help,onCreate,2(o247):
Help,onClick,3(o247):
Score,onCreate,2(o247):
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
Score,onClick,3(o247):
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
Activity,Activity,1(o245):
Activity,intent,2(o245):
Activity,getIntent,3(o245):
Activity,onCreate,4(o245):
Activity,onClick,5(o245):
Activity,addButton,6(o245):
Activity,startActivity,7(o245):
Intent,Intent,1(o245):
Intent,target,2(o245):
Intent,data,3(o245):
Intent,setTarget,4(o245):
Intent,setData,5(o245):
Intent,getData,6(o245):
Main,Main,1(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,Game,1(o245):
Game,onCreate,2(o245):
Game,onClick,3(o245):
Help,Help,1(o245):
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,Score,1(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Main,onCreate,2(o245):
Main,onClick,3(o245):
Game,onCreate,2(o245):
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
Game,onClick,3(o245):
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
Help,onCreate,2(o245):
Help,onClick,3(o245):
Score,onCreate,2(o245):
Score,onClick,3(o245):
Activity,Activity,1(o180):
Activity,intent,2(o180):
Activity,getIntent,3(o180):
Activity,onCreate,4(o180):
Activity,onClick,5(o180):
Activity,addButton,6(o180):
Activity,startActivity,7(o180):
Intent,Intent,1(o180):
Intent,target,2(o180):
Intent,data,3(o180):
Intent,setTarget,4(o180):
Intent,setData,5(o180):
Intent,getData,6(o180):
Main,Main,1(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,Game,1(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,Help,1(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,Score,1(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Main,onCreate,2(o180):
Main,onClick,3(o180):
Game,onCreate,2(o180):
Game,onClick,3(o180):
Help,onCreate,2(o180):
Help,onClick,3(o180):
Score,onCreate,2(o180):
Score,onClick,3(o180):
Activity,Activity,1(o176):
Activity,intent,2(o176):
Activity,getIntent,3(o176):
Activity,onCreate,4(o176):
Activity,onClick,5(o176):
Activity,addButton,6(o176):
Activity,startActivity,7(o176):
Intent,Intent,1(o176):
Intent,target,2(o176):
Intent,data,3(o176):
Intent,setTarget,4(o176):
Intent,setData,5(o176):
Intent,getData,6(o176):
Main,Main,1(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,Game,1(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,Help,1(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,Score,1(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Main,onCreate,2(o176):
Main,onClick,3(o176):
Game,onCreate,2(o176):
Game,onClick,3(o176):
Help,onCreate,2(o176):
Help,onClick,3(o176):
Score,onCreate,2(o176):
Score,onClick,3(o176):
Activity,Activity,1(o174):
Activity,intent,2(o174):
Activity,getIntent,3(o174):
Activity,onCreate,4(o174):
Activity,onClick,5(o174):
Activity,addButton,6(o174):
Activity,startActivity,7(o174):
Intent,Intent,1(o174):
Intent,target,2(o174):
Intent,data,3(o174):
Intent,setTarget,4(o174):
Intent,setData,5(o174):
Intent,getData,6(o174):
Main,Main,1(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,Game,1(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,Help,1(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,Score,1(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Main,onCreate,2(o174):
Main,onClick,3(o174):
Game,onCreate,2(o174):
Game,onClick,3(o174):
Help,onCreate,2(o174):
Help,onClick,3(o174):
Score,onCreate,2(o174):
Score,onClick,3(o174):
Activity,Activity,1(o165):
Activity,intent,2(o165):
Activity,getIntent,3(o165):
Activity,onCreate,4(o165):
Activity,onClick,5(o165):
Activity,addButton,6(o165):
Activity,startActivity,7(o165):
Intent,Intent,1(o165):
Intent,target,2(o165):
Intent,data,3(o165):
Intent,setTarget,4(o165):
Intent,setData,5(o165):
Intent,getData,6(o165):
Main,Main,1(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,Game,1(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,Help,1(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,Score,1(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Main,onCreate,2(o165):
Main,onClick,3(o165):
Game,onCreate,2(o165):
Game,onClick,3(o165):
Help,onCreate,2(o165):
Help,onClick,3(o165):
Score,onCreate,2(o165):
Score,onClick,3(o165):
Activity,Activity,1(o163):
Activity,intent,2(o163):
Activity,getIntent,3(o163):
Activity,onCreate,4(o163):
Activity,onClick,5(o163):
Activity,addButton,6(o163):
Activity,startActivity,7(o163):
Intent,Intent,1(o163):
Intent,target,2(o163):
Intent,data,3(o163):
Intent,setTarget,4(o163):
Intent,setData,5(o163):
Intent,getData,6(o163):
Main,Main,1(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,Game,1(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,Help,1(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,Score,1(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Main,onCreate,2(o163):
Main,onClick,3(o163):
Game,onCreate,2(o163):
Game,onClick,3(o163):
Help,onCreate,2(o163):
Help,onClick,3(o163):
Score,onCreate,2(o163):
Score,onClick,3(o163):
Activity,Activity,1(o154):
Activity,intent,2(o154):
Activity,getIntent,3(o154):
Activity,onCreate,4(o154):
Activity,onClick,5(o154):
Activity,addButton,6(o154):
Activity,startActivity,7(o154):
Intent,Intent,1(o154):
Intent,target,2(o154):
Intent,data,3(o154):
Intent,setTarget,4(o154):
Intent,setData,5(o154):
Intent,getData,6(o154):
Main,Main,1(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,Game,1(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,Help,1(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,Score,1(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Main,onCreate,2(o154):
Main,onClick,3(o154):
Game,onCreate,2(o154):
Game,onClick,3(o154):
Help,onCreate,2(o154):
Help,onClick,3(o154):
Score,onCreate,2(o154):
Score,onClick,3(o154):
Activity,Activity,1(o152):
Activity,intent,2(o152):
Activity,getIntent,3(o152):
Activity,onCreate,4(o152):
Activity,onClick,5(o152):
Activity,addButton,6(o152):
Activity,startActivity,7(o152):
Intent,Intent,1(o152):
Intent,target,2(o152):
Intent,data,3(o152):
Intent,setTarget,4(o152):
Intent,setData,5(o152):
Intent,getData,6(o152):
Main,Main,1(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,Game,1(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,Help,1(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,Score,1(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Main,onCreate,2(o152):
Main,onClick,3(o152):
Game,onCreate,2(o152):
Game,onClick,3(o152):
Help,onCreate,2(o152):
Help,onClick,3(o152):
Score,onCreate,2(o152):
Score,onClick,3(o152):
Generating objects [1 iterations]

Initial constraints
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
 - (int{x552})--e554-->void{x553} <: (int{x520})--e522-->void{x521}
 - ()--e539-->void{x538} <: ()--e519-->void{x518}
 - {} U {} U {} U {} U e558 U {} U {} U {} U {} U e561 U {} U {} U {} U e563 U {} U {} U {} U {} U e567 U {} U {} U {} U {} U e570 U {} U {} U {} U e572 U {} U {} U {} U {} U e576 U {} U {} U {} U {} U e579 U {} U {} U {} U e582 U {} U {} U {} U e584 U {} <= e554
 - Main{x534}.startActivity <: (Intent{x551})--e584-->void{x583}
 - Intent{x551}.setData <: (String{x580})--e582-->void{x581}
 - { o180 } <= x580
 - Intent{x551}.setTarget <: (String{x577})--e579-->void{x578}
 - { o176 } <= x577
 - Intent{x575} <: Intent{x551}
 - Intent{x575}.Intent <: ()--e576-->Intent{x575}
 - { o174 } <= x575
 - Main{x534}.startActivity <: (Intent{x550})--e572-->void{x571}
 - Intent{x550}.setTarget <: (String{x568})--e570-->void{x569}
 - { o165 } <= x568
 - Intent{x566} <: Intent{x550}
 - Intent{x566}.Intent <: ()--e567-->Intent{x566}
 - { o163 } <= x566
 - Main{x534}.startActivity <: (Intent{x549})--e563-->void{x562}
 - Intent{x549}.setTarget <: (String{x559})--e561-->void{x560}
 - { o154 } <= x559
 - Intent{x557} <: Intent{x549}
 - Intent{x557}.Intent <: ()--e558-->Intent{x557}
 - { o152 } <= x557
 - { o506 } <= x534
 - {} U {} U {} U e542 U {} U {} U {} U e545 U {} U {} U {} U e548 <= e539
 - Main{x534}.addButton <: (int{x546})--e548-->void{x547}
 - Main{x534}.addButton <: (int{x543})--e545-->void{x544}
 - Main{x534}.addButton <: (int{x540})--e542-->void{x541}
 - e537 U {} <= e535
 - { o506 } <= x536
 - Activity{x536}.Activity <: ()--e537-->Activity{x536}
 - { o506 } <= x533
 - {} U {} U e531 <= e530
 - Intent{x528} ~~~> e531 @ [{o506},ActivitystartActivity,7,1, n/a]
 - { o506 } <= x509
 - {} U {} U e526 <= e525
 - {} <= e522
 - {} <= e519
 - {} <= e516
 - Intent{x517} <: Intent{x515}
 - Activity{x509}.intent <: Intent{x517}
 - e514 U {} <= e512
 - { o506 } <= x513
 - Object{x513}.Object <: ()--e514-->Object{x513}
 - { o506 } <= x508
 - null{x510} <: Activity{x509}.intent
 - {} <= e504
 - Object{x505} <: Object{x503}
 - Intent{x487}.data <: Object{x505}
 - Object{x491} <: Object{x505}
 - { o295 } <= x487
 - {} U {} <= e501
 - {  } <= x502
 - Object{x499} <: Intent{x487}.data
 - Object{x499} <: Object{x491}
 - {} U {} <= e497
 - {  } <= x498
 - String{x495} <: Intent{x487}.target
 - String{x495} <: String{x489}
 - e494 U {} <= e492
 - { o295 } <= x493
 - Object{x493}.Object <: ()--e494-->Object{x493}
 - { o295 } <= x486
 - null{x490} <: Intent{x487}.data
 - null{x490} <: Object{x491}
 - null{x488} <: Intent{x487}.target
 - null{x488} <: String{x489}
 - {} <= e484
 - Object{x485} <: Object{x483}
 - Intent{x467}.data <: Object{x485}
 - Object{x471} <: Object{x485}
 - { o345 } <= x467
 - {} U {} <= e481
 - {  } <= x482
 - Object{x479} <: Intent{x467}.data
 - Object{x479} <: Object{x471}
 - {} U {} <= e477
 - {  } <= x478
 - String{x475} <: Intent{x467}.target
 - String{x475} <: String{x469}
 - e474 U {} <= e472
 - { o345 } <= x473
 - Object{x473}.Object <: ()--e474-->Object{x473}
 - { o345 } <= x466
 - null{x470} <: Intent{x467}.data
 - null{x470} <: Object{x471}
 - null{x468} <: Intent{x467}.target
 - null{x468} <: String{x469}
 - {} <= e464
 - Object{x465} <: Object{x463}
 - Intent{x447}.data <: Object{x465}
 - Object{x451} <: Object{x465}
 - { o402 } <= x447
 - {} U {} <= e461
 - {  } <= x462
 - Object{x459} <: Intent{x447}.data
 - Object{x459} <: Object{x451}
 - {} U {} <= e457
 - {  } <= x458
 - String{x455} <: Intent{x447}.target
 - String{x455} <: String{x449}
 - e454 U {} <= e452
 - { o402 } <= x453
 - Object{x453}.Object <: ()--e454-->Object{x453}
 - { o402 } <= x446
 - null{x450} <: Intent{x447}.data
 - null{x450} <: Object{x451}
 - null{x448} <: Intent{x447}.target
 - null{x448} <: String{x449}
 - {} <= e444
 - Object{x445} <: Object{x443}
 - Intent{x427}.data <: Object{x445}
 - Object{x431} <: Object{x445}
 - { o417 } <= x427
 - {} U {} <= e441
 - {  } <= x442
 - Object{x439} <: Intent{x427}.data
 - Object{x439} <: Object{x431}
 - {} U {} <= e437
 - {  } <= x438
 - String{x435} <: Intent{x427}.target
 - String{x435} <: String{x429}
 - e434 U {} <= e432
 - { o417 } <= x433
 - Object{x433}.Object <: ()--e434-->Object{x433}
 - { o417 } <= x426
 - null{x430} <: Intent{x427}.data
 - null{x430} <: Object{x431}
 - null{x428} <: Intent{x427}.target
 - null{x428} <: String{x429}
 - {} U {} U {} U {} U e401 U {} U {} U {} U {} U e406 U {} U {} U {} U e410 U {} U {} U {} U e412 U {} U {} U {} U {} U e416 U {} U {} U {} U {} U e421 U {} U {} U {} U e423 U {} U {} U {} U {} U {} <= e397
 - Game{x378}.startActivity <: (Intent{x394})--e423-->void{x422}
 - (Intent{x372})--e374-->void{x373} <: (Intent{x394})--e423-->void{x422}
 - Intent{x394}.setTarget <: (String{x418})--e421-->void{x420}
 - (String{x435})--e437-->void{x436} <: (String{x418})--e421-->void{x420}
 - { o419 } <= x418
 - Intent{x415} <: Intent{x394}
 - Intent{x415}.Intent <: ()--e416-->Intent{x415}
 - ()--e432-->Intent{x426} <: ()--e416-->Intent{x415}
 - { o417 } <= x415
 - Game{x378}.startActivity <: (Intent{x393})--e412-->void{x411}
 - (Intent{x372})--e374-->void{x373} <: (Intent{x393})--e412-->void{x411}
 - Intent{x393}.setData <: (String{x407})--e410-->void{x409}
 - (Object{x459})--e461-->void{x460} <: (String{x407})--e410-->void{x409}
 - { o408 } <= x407
 - Intent{x393}.setTarget <: (String{x403})--e406-->void{x405}
 - (String{x455})--e457-->void{x456} <: (String{x403})--e406-->void{x405}
 - { o404 } <= x403
 - Intent{x400} <: Intent{x393}
 - Intent{x400}.Intent <: ()--e401-->Intent{x400}
 - ()--e452-->Intent{x446} <: ()--e401-->Intent{x400}
 - { o402 } <= x400
 - { o245 } <= x378
 - {} U {} U {} U e386 U {} U {} U {} U e389 U {} U {} U {} U e392 <= e383
 - Game{x378}.addButton <: (int{x390})--e392-->void{x391}
 - (int{x367})--e369-->void{x368} <: (int{x390})--e392-->void{x391}
 - Game{x378}.addButton <: (int{x387})--e389-->void{x388}
 - (int{x367})--e369-->void{x368} <: (int{x387})--e389-->void{x388}
 - Game{x378}.addButton <: (int{x384})--e386-->void{x385}
 - (int{x367})--e369-->void{x368} <: (int{x384})--e386-->void{x385}
 - e381 U {} <= e379
 - { o245 } <= x380
 - Activity{x380}.Activity <: ()--e381-->Activity{x380}
 - ()--e356-->Activity{x352} <: ()--e381-->Activity{x380}
 - { o245 } <= x377
 - {} U {} U e375 <= e374
 - Intent{x372} ~~~> e375 @ [{o245},ActivitystartActivity,7,1, n/a]
 - { o249 } <= x250
 - Intent{x372} <: Help{x250}.intent
 - { o247 } <= x248
 - Intent{x372} <: Score{x248}.intent
 - {Help,Score} <= e375
 - Intent{x372} <: Intent{x254}
 - Intent{x372} <: Intent{x307}
 - {} <= e375
 - { o245 } <= x353
 - {} U {} U e370 <= e369
 - {} <= e366
 - {} <= e363
 - {} <= e360
 - Intent{x361} <: Intent{x359}
 - Activity{x353}.intent <: Intent{x361}
 - Intent{x355} <: Intent{x361}
 - e358 U {} <= e356
 - { o245 } <= x357
 - Object{x357}.Object <: ()--e358-->Object{x357}
 - { o245 } <= x352
 - null{x354} <: Activity{x353}.intent
 - null{x354} <: Intent{x355}
 - {} U e344 U {} U {} U {} U {} U e349 U {} U {} U {} U e351 <= e342
 - Score{x330}.startActivity <: (Intent{x339})--e351-->void{x350}
 - (Intent{x324})--e326-->void{x325} <: (Intent{x339})--e351-->void{x350}
 - Intent{x339}.setTarget <: (String{x346})--e349-->void{x348}
 - (String{x475})--e477-->void{x476} <: (String{x346})--e349-->void{x348}
 - { o347 } <= x346
 - Intent{x343} <: Intent{x339}
 - Intent{x343}.Intent <: ()--e344-->Intent{x343}
 - ()--e472-->Intent{x466} <: ()--e344-->Intent{x343}
 - { o345 } <= x343
 - { o247 } <= x330
 - {} U {} U {} U e338 <= e335
 - Score{x330}.addButton <: (int{x336})--e338-->void{x337}
 - (int{x319})--e321-->void{x320} <: (int{x336})--e338-->void{x337}
 - e333 U {} <= e331
 - { o247 } <= x332
 - Activity{x332}.Activity <: ()--e333-->Activity{x332}
 - ()--e308-->Activity{x304} <: ()--e333-->Activity{x332}
 - { o247 } <= x329
 - {} U {} U e327 <= e326
 - Intent{x324} ~~~> e327 @ [{o247},ActivitystartActivity,7,1, n/a]
 - { o506 } <= x507
 - Intent{x324} <: Main{x507}.intent
 - {Main} <= e327
 - {} <= e327
 - { o247 } <= x305
 - {} U {} U e322 <= e321
 - {} <= e318
 - {} <= e315
 - {} <= e312
 - Intent{x313} <: Intent{x311}
 - Activity{x305}.intent <: Intent{x313}
 - Intent{x307} <: Intent{x313}
 - e310 U {} <= e308
 - { o247 } <= x309
 - Object{x309}.Object <: ()--e310-->Object{x309}
 - { o247 } <= x304
 - null{x306} <: Activity{x305}.intent
 - null{x306} <: Intent{x307}
 - {} U e294 U {} U {} U {} U e297 U {} U {} U e299 U {} U {} U {} U {} U e301 U {} U {} U {} U e303 <= e292
 - Help{x277}.startActivity <: (Intent{x286})--e303-->void{x302}
 - (Intent{x271})--e273-->void{x272} <: (Intent{x286})--e303-->void{x302}
 - Intent{x286}.setTarget <: (String{x289})--e301-->void{x300}
 - (String{x495})--e497-->void{x496} <: (String{x289})--e301-->void{x300}
 - String{x288} <: String{x289}
 - Object{x298} <: Object{x288}
 - Intent{x287}.getData <: ()--e299-->Object{x298}
 - ()--e243-->Object{x242} <: ()--e299-->Object{x298}
 - ()--e223-->Object{x222} <: ()--e299-->Object{x298}
 - ()--e203-->Object{x202} <: ()--e299-->Object{x298}
 - ()--e464-->Object{x463} <: ()--e299-->Object{x298}
 - ()--e444-->Object{x443} <: ()--e299-->Object{x298}
 - Intent{x296} <: Intent{x287}
 - Help{x277}.getIntent <: ()--e297-->Intent{x296}
 - ()--e259-->Intent{x258} <: ()--e297-->Intent{x296}
 - Intent{x293} <: Intent{x286}
 - Intent{x293}.Intent <: ()--e294-->Intent{x293}
 - ()--e492-->Intent{x486} <: ()--e294-->Intent{x293}
 - { o295 } <= x293
 - { o249 } <= x277
 - {} U {} U {} U e285 <= e282
 - Help{x277}.addButton <: (int{x283})--e285-->void{x284}
 - (int{x266})--e268-->void{x267} <: (int{x283})--e285-->void{x284}
 - e280 U {} <= e278
 - { o249 } <= x279
 - Activity{x279}.Activity <: ()--e280-->Activity{x279}
 - ()--e255-->Activity{x251} <: ()--e280-->Activity{x279}
 - { o249 } <= x276
 - {} U {} U e274 <= e273
 - Intent{x271} ~~~> e274 @ [{o249},ActivitystartActivity,7,1, n/a]
 - Intent{x271} <: Main{x507}.intent
 - { o245 } <= x246
 - Intent{x271} <: Game{x246}.intent
 - {Main,Game} <= e274
 - Intent{x271} <: Intent{x355}
 - {Main} <= e274
 - {} <= e274
 - { o249 } <= x252
 - {} U {} U e269 <= e268
 - {} <= e265
 - {} <= e262
 - {} <= e259
 - Intent{x260} <: Intent{x258}
 - Activity{x252}.intent <: Intent{x260}
 - Intent{x254} <: Intent{x260}
 - e257 U {} <= e255
 - { o249 } <= x256
 - Object{x256}.Object <: ()--e257-->Object{x256}
 - { o249 } <= x251
 - null{x253} <: Activity{x252}.intent
 - null{x253} <: Intent{x254}
 - {} <= e243
 - Object{x244} <: Object{x242}
 - Intent{x226}.data <: Object{x244}
 - Object{x230} <: Object{x244}
 - { o152 } <= x226
 - {} U {} <= e240
 - {  } <= x241
 - Object{x238} <: Intent{x226}.data
 - Object{x238} <: Object{x230}
 - {} U {} <= e236
 - {  } <= x237
 - String{x234} <: Intent{x226}.target
 - String{x234} <: String{x228}
 - e233 U {} <= e231
 - { o152 } <= x232
 - Object{x232}.Object <: ()--e233-->Object{x232}
 - { o152 } <= x225
 - null{x229} <: Intent{x226}.data
 - null{x229} <: Object{x230}
 - null{x227} <: Intent{x226}.target
 - null{x227} <: String{x228}
 - {} <= e223
 - Object{x224} <: Object{x222}
 - Intent{x206}.data <: Object{x224}
 - Object{x210} <: Object{x224}
 - { o163 } <= x206
 - {} U {} <= e220
 - {  } <= x221
 - Object{x218} <: Intent{x206}.data
 - Object{x218} <: Object{x210}
 - {} U {} <= e216
 - {  } <= x217
 - String{x214} <: Intent{x206}.target
 - String{x214} <: String{x208}
 - e213 U {} <= e211
 - { o163 } <= x212
 - Object{x212}.Object <: ()--e213-->Object{x212}
 - { o163 } <= x205
 - null{x209} <: Intent{x206}.data
 - null{x209} <: Object{x210}
 - null{x207} <: Intent{x206}.target
 - null{x207} <: String{x208}
 - {} <= e203
 - Object{x204} <: Object{x202}
 - Intent{x186}.data <: Object{x204}
 - Object{x190} <: Object{x204}
 - { o174 } <= x186
 - {} U {} <= e200
 - {  } <= x201
 - Object{x198} <: Intent{x186}.data
 - Object{x198} <: Object{x190}
 - {} U {} <= e196
 - {  } <= x197
 - String{x194} <: Intent{x186}.target
 - String{x194} <: String{x188}
 - e193 U {} <= e191
 - { o174 } <= x192
 - Object{x192}.Object <: ()--e193-->Object{x192}
 - { o174 } <= x185
 - null{x189} <: Intent{x186}.data
 - null{x189} <: Object{x190}
 - null{x187} <: Intent{x186}.target
 - null{x187} <: String{x188}
 - (int{x145})--e147-->void{x146} <: (int{x113})--e115-->void{x114}
 - ()--e132-->void{x131} <: ()--e112-->void{x111}
 - {} U {} U {} U {} U e151 U {} U {} U {} U {} U e156 U {} U {} U {} U e158 U {} U {} U {} U {} U e162 U {} U {} U {} U {} U e167 U {} U {} U {} U e169 U {} U {} U {} U {} U e173 U {} U {} U {} U {} U e178 U {} U {} U {} U e182 U {} U {} U {} U e184 U {} <= e147
 - Main{x127}.startActivity <: (Intent{x144})--e184-->void{x183}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x144})--e184-->void{x183}
 - Intent{x144}.setData <: (String{x179})--e182-->void{x181}
 - (Object{x198})--e200-->void{x199} <: (String{x179})--e182-->void{x181}
 - { o180 } <= x179
 - Intent{x144}.setTarget <: (String{x175})--e178-->void{x177}
 - (String{x194})--e196-->void{x195} <: (String{x175})--e178-->void{x177}
 - { o176 } <= x175
 - Intent{x172} <: Intent{x144}
 - Intent{x172}.Intent <: ()--e173-->Intent{x172}
 - ()--e191-->Intent{x185} <: ()--e173-->Intent{x172}
 - { o174 } <= x172
 - Main{x127}.startActivity <: (Intent{x143})--e169-->void{x168}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x143})--e169-->void{x168}
 - Intent{x143}.setTarget <: (String{x164})--e167-->void{x166}
 - (String{x214})--e216-->void{x215} <: (String{x164})--e167-->void{x166}
 - { o165 } <= x164
 - Intent{x161} <: Intent{x143}
 - Intent{x161}.Intent <: ()--e162-->Intent{x161}
 - ()--e211-->Intent{x205} <: ()--e162-->Intent{x161}
 - { o163 } <= x161
 - Main{x127}.startActivity <: (Intent{x142})--e158-->void{x157}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x142})--e158-->void{x157}
 - Intent{x142}.setTarget <: (String{x153})--e156-->void{x155}
 - (String{x234})--e236-->void{x235} <: (String{x153})--e156-->void{x155}
 - { o154 } <= x153
 - Intent{x150} <: Intent{x142}
 - Intent{x150}.Intent <: ()--e151-->Intent{x150}
 - ()--e231-->Intent{x225} <: ()--e151-->Intent{x150}
 - { o152 } <= x150
 - { o1 } <= x127
 - {} U {} U {} U e135 U {} U {} U {} U e138 U {} U {} U {} U e141 <= e132
 - Main{x127}.addButton <: (int{x139})--e141-->void{x140}
 - (int{x116})--e118-->void{x117} <: (int{x139})--e141-->void{x140}
 - Main{x127}.addButton <: (int{x136})--e138-->void{x137}
 - (int{x116})--e118-->void{x117} <: (int{x136})--e138-->void{x137}
 - Main{x127}.addButton <: (int{x133})--e135-->void{x134}
 - (int{x116})--e118-->void{x117} <: (int{x133})--e135-->void{x134}
 - e130 U {} <= e128
 - { o1 } <= x129
 - Activity{x129}.Activity <: ()--e130-->Activity{x129}
 - ()--e105-->Activity{x101} <: ()--e130-->Activity{x129}
 - { o1 } <= x126
 - {} U {} U e124 <= e123
 - Intent{x121} ~~~> e124 @ [{o1},ActivitystartActivity,7,1, n/a]
 - Intent{x121} <: Game{x246}.intent
 - Intent{x121} <: Score{x248}.intent
 - Intent{x121} <: Help{x250}.intent
 - {Game,Score,Help} <= e124
 - Intent{x121} <: Intent{x355}
 - Intent{x121} <: Intent{x307}
 - Intent{x121} <: Intent{x254}
 - {} <= e124
 - { o1 } <= x102
 - {} U {} U e119 <= e118
 - {} <= e115
 - {} <= e112
 - {} <= e109
 - Intent{x110} <: Intent{x108}
 - Activity{x102}.intent <: Intent{x110}
 - Intent{x104} <: Intent{x110}
 - e107 U {} <= e105
 - { o1 } <= x106
 - Object{x106}.Object <: ()--e107-->Object{x106}
 - { o1 } <= x101
 - null{x103} <: Activity{x102}.intent
 - null{x103} <: Intent{x104}

. . . 
Solving Constraints [3 iterations]
The solution is:
 - x101 = { o1 }
 - x102 = { o1 }
 - x104 = {  }
 - x106 = { o1 }
 - x108 = {  }
 - x110 = {  }
 - x111 = {  }
 - x114 = {  }
 - x116 = {  }
 - x121 = { o152,o163,o174 }
 - x126 = { o1 }
 - x127 = { o1 }
 - x129 = { o1 }
 - x134 = {  }
 - x137 = {  }
 - x140 = {  }
 - x142 = { o152 }
 - x143 = { o163 }
 - x144 = { o174 }
 - x145 = {  }
 - x150 = { o152 }
 - x153 = { o154 }
 - x155 = {  }
 - x157 = {  }
 - x161 = { o163 }
 - x164 = { o165 }
 - x166 = {  }
 - x168 = {  }
 - x172 = { o174 }
 - x175 = { o176 }
 - x177 = {  }
 - x179 = { o180 }
 - x181 = {  }
 - x183 = {  }
 - x185 = { o174 }
 - x186 = { o174 }
 - x188 = { o176 }
 - x190 = { o180 }
 - x192 = { o174 }
 - x194 = { o176 }
 - x197 = {  }
 - x198 = { o180 }
 - x201 = {  }
 - x202 = { o180 }
 - x204 = { o180 }
 - x205 = { o163 }
 - x206 = { o163 }
 - x208 = { o165 }
 - x210 = {  }
 - x212 = { o163 }
 - x214 = { o165 }
 - x217 = {  }
 - x221 = {  }
 - x222 = {  }
 - x224 = {  }
 - x225 = { o152 }
 - x226 = { o152 }
 - x228 = { o154 }
 - x230 = {  }
 - x232 = { o152 }
 - x234 = { o154 }
 - x237 = {  }
 - x241 = {  }
 - x242 = {  }
 - x244 = {  }
 - x246 = { o245 }
 - x248 = { o247 }
 - x250 = { o249 }
 - x251 = { o249 }
 - x252 = { o249 }
 - x254 = { o152,o163,o174,o402,o417 }
 - x256 = { o249 }
 - x258 = { o152,o163,o174,o402,o417 }
 - x260 = { o152,o163,o174,o402,o417 }
 - x261 = {  }
 - x264 = {  }
 - x266 = {  }
 - x271 = { o295 }
 - x276 = { o249 }
 - x277 = { o249 }
 - x279 = { o249 }
 - x284 = {  }
 - x286 = { o295 }
 - x287 = { o152,o163,o174,o402,o417 }
 - x288 = { o180,o408 }
 - x289 = { o180,o408 }
 - x290 = {  }
 - x293 = { o295 }
 - x296 = { o152,o163,o174,o402,o417 }
 - x298 = { o180,o408 }
 - x300 = {  }
 - x302 = {  }
 - x304 = { o247 }
 - x305 = { o247 }
 - x307 = { o152,o163,o174,o402,o417 }
 - x309 = { o247 }
 - x311 = { o152,o163,o174,o402,o417 }
 - x313 = { o152,o163,o174,o402,o417 }
 - x314 = {  }
 - x317 = {  }
 - x319 = {  }
 - x324 = { o345 }
 - x329 = { o247 }
 - x330 = { o247 }
 - x332 = { o247 }
 - x337 = {  }
 - x339 = { o345 }
 - x340 = {  }
 - x343 = { o345 }
 - x346 = { o347 }
 - x348 = {  }
 - x350 = {  }
 - x352 = { o245 }
 - x353 = { o245 }
 - x355 = { o152,o163,o174,o295 }
 - x357 = { o245 }
 - x359 = { o152,o163,o174,o295 }
 - x361 = { o152,o163,o174,o295 }
 - x362 = {  }
 - x365 = {  }
 - x367 = {  }
 - x372 = { o402,o417 }
 - x377 = { o245 }
 - x378 = { o245 }
 - x380 = { o245 }
 - x385 = {  }
 - x388 = {  }
 - x391 = {  }
 - x393 = { o402 }
 - x394 = { o417 }
 - x395 = {  }
 - x400 = { o402 }
 - x403 = { o404 }
 - x405 = {  }
 - x407 = { o408 }
 - x409 = {  }
 - x411 = {  }
 - x415 = { o417 }
 - x418 = { o419 }
 - x420 = {  }
 - x422 = {  }
 - x426 = { o417 }
 - x427 = { o417 }
 - x429 = { o419 }
 - x431 = {  }
 - x433 = { o417 }
 - x435 = { o419 }
 - x438 = {  }
 - x442 = {  }
 - x443 = {  }
 - x445 = {  }
 - x446 = { o402 }
 - x447 = { o402 }
 - x449 = { o404 }
 - x451 = { o408 }
 - x453 = { o402 }
 - x455 = { o404 }
 - x458 = {  }
 - x459 = { o408 }
 - x462 = {  }
 - x463 = { o408 }
 - x465 = { o408 }
 - x466 = { o345 }
 - x467 = { o345 }
 - x469 = { o347 }
 - x471 = {  }
 - x473 = { o345 }
 - x475 = { o347 }
 - x478 = {  }
 - x482 = {  }
 - x483 = {  }
 - x485 = {  }
 - x486 = { o295 }
 - x487 = { o295 }
 - x489 = { o180,o408 }
 - x491 = {  }
 - x493 = { o295 }
 - x495 = { o180,o408 }
 - x498 = {  }
 - x502 = {  }
 - x503 = {  }
 - x505 = {  }
 - x507 = { o506 }
 - x508 = { o506 }
 - x509 = { o506 }
 - x511 = { o295,o345 }
 - x513 = { o506 }
 - x515 = { o295,o345 }
 - x517 = { o295,o345 }
 - x518 = {  }
 - x521 = {  }
 - x523 = {  }
 - x528 = { o152,o163,o174 }
 - x533 = { o506 }
 - x534 = { o506 }
 - x536 = { o506 }
 - x541 = {  }
 - x544 = {  }
 - x547 = {  }
 - x549 = { o152 }
 - x550 = { o163 }
 - x551 = { o174 }
 - x552 = {  }
 - x557 = { o152 }
 - x559 = { o154 }
 - x560 = {  }
 - x562 = {  }
 - x566 = { o163 }
 - x568 = { o165 }
 - x569 = {  }
 - x571 = {  }
 - x575 = { o174 }
 - x577 = { o176 }
 - x578 = {  }
 - x580 = { o180 }
 - x581 = {  }
 - x583 = {  }

 - e105 = {}
 - e109 = {}
 - e112 = {}
 - e115 = {Game,Score,Help}
 - e118 = {}
 - e123 = {Game,Score,Help}
 - e124 = {Game,Score,Help}
 - e128 = {}
 - e130 = {}
 - e132 = {}
 - e135 = {}
 - e138 = {}
 - e141 = {}
 - e147 = {Game,Score,Help}
 - e151 = {}
 - e156 = {}
 - e158 = {Game,Score,Help}
 - e162 = {}
 - e167 = {}
 - e169 = {Game,Score,Help}
 - e173 = {}
 - e178 = {}
 - e182 = {}
 - e184 = {Game,Score,Help}
 - e191 = {}
 - e196 = {}
 - e200 = {}
 - e203 = {}
 - e211 = {}
 - e216 = {}
 - e220 = {}
 - e223 = {}
 - e231 = {}
 - e236 = {}
 - e240 = {}
 - e243 = {}
 - e255 = {}
 - e259 = {}
 - e262 = {}
 - e265 = {Main,Game}
 - e268 = {}
 - e273 = {Main,Game}
 - e274 = {Main,Game}
 - e278 = {}
 - e280 = {}
 - e282 = {}
 - e285 = {}
 - e292 = {Main,Game}
 - e294 = {}
 - e297 = {}
 - e299 = {}
 - e301 = {}
 - e303 = {Main,Game}
 - e308 = {}
 - e312 = {}
 - e315 = {}
 - e318 = {Main}
 - e321 = {}
 - e326 = {Main}
 - e327 = {Main}
 - e331 = {}
 - e333 = {}
 - e335 = {}
 - e338 = {}
 - e342 = {Main}
 - e344 = {}
 - e349 = {}
 - e351 = {Main}
 - e356 = {}
 - e360 = {}
 - e363 = {}
 - e366 = {Help,Score}
 - e369 = {}
 - e374 = {Help,Score}
 - e375 = {Help,Score}
 - e379 = {}
 - e381 = {}
 - e383 = {}
 - e386 = {}
 - e389 = {}
 - e392 = {}
 - e397 = {Help,Score}
 - e401 = {}
 - e406 = {}
 - e410 = {}
 - e412 = {Help,Score}
 - e416 = {}
 - e421 = {}
 - e423 = {Help,Score}
 - e432 = {}
 - e437 = {}
 - e441 = {}
 - e444 = {}
 - e452 = {}
 - e457 = {}
 - e461 = {}
 - e464 = {}
 - e472 = {}
 - e477 = {}
 - e481 = {}
 - e484 = {}
 - e492 = {}
 - e497 = {}
 - e501 = {}
 - e504 = {}
 - e512 = {}
 - e516 = {}
 - e519 = {}
 - e522 = {Game,Score,Help}
 - e525 = {}
 - e530 = {Game,Score,Help}
 - e531 = {Game,Score,Help}
 - e535 = {}
 - e537 = {}
 - e539 = {}
 - e542 = {}
 - e545 = {}
 - e548 = {}
 - e554 = {Game,Score,Help}
 - e558 = {}
 - e561 = {}
 - e563 = {Game,Score,Help}
 - e567 = {}
 - e570 = {}
 - e572 = {Game,Score,Help}
 - e576 = {}
 - e579 = {}
 - e582 = {}
 - e584 = {Game,Score,Help}

Total 3 iterations
The final set of constraints:
 - null{x103} <: Intent{x104}
 - null{x103} <: Activity{x102}.intent
 - { o1 } <= x101
 - Object{x106}.Object <: ()--e107-->Object{x106}
 - { o1 } <= x106
 - e107 U {} <= e105
 - Intent{x104} <: Intent{x110}
 - Activity{x102}.intent <: Intent{x110}
 - Intent{x110} <: Intent{x108}
 - {} <= e109
 - {} <= e112
 - {} <= e115
 - {} U {} U e119 <= e118
 - { o1 } <= x102
 - {} <= e124
 - Intent{x121} <: Intent{x254}
 - Intent{x121} <: Intent{x307}
 - Intent{x121} <: Intent{x355}
 - {Game,Score,Help} <= e124
 - Intent{x121} <: Help{x250}.intent
 - Intent{x121} <: Score{x248}.intent
 - Intent{x121} <: Game{x246}.intent
 - Intent{x121} ~~~> e124 @ [{o1},ActivitystartActivity,7,1, n/a]
 - {} U {} U e124 <= e123
 - { o1 } <= x126
 - ()--e105-->Activity{x101} <: ()--e130-->Activity{x129}
 - Activity{x129}.Activity <: ()--e130-->Activity{x129}
 - { o1 } <= x129
 - e130 U {} <= e128
 - (int{x116})--e118-->void{x117} <: (int{x133})--e135-->void{x134}
 - Main{x127}.addButton <: (int{x133})--e135-->void{x134}
 - (int{x116})--e118-->void{x117} <: (int{x136})--e138-->void{x137}
 - Main{x127}.addButton <: (int{x136})--e138-->void{x137}
 - (int{x116})--e118-->void{x117} <: (int{x139})--e141-->void{x140}
 - Main{x127}.addButton <: (int{x139})--e141-->void{x140}
 - {} U {} U {} U e135 U {} U {} U {} U e138 U {} U {} U {} U e141 <= e132
 - { o1 } <= x127
 - { o152 } <= x150
 - ()--e231-->Intent{x225} <: ()--e151-->Intent{x150}
 - Intent{x150}.Intent <: ()--e151-->Intent{x150}
 - Intent{x150} <: Intent{x142}
 - { o154 } <= x153
 - (String{x234})--e236-->void{x235} <: (String{x153})--e156-->void{x155}
 - Intent{x142}.setTarget <: (String{x153})--e156-->void{x155}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x142})--e158-->void{x157}
 - Main{x127}.startActivity <: (Intent{x142})--e158-->void{x157}
 - { o163 } <= x161
 - ()--e211-->Intent{x205} <: ()--e162-->Intent{x161}
 - Intent{x161}.Intent <: ()--e162-->Intent{x161}
 - Intent{x161} <: Intent{x143}
 - { o165 } <= x164
 - (String{x214})--e216-->void{x215} <: (String{x164})--e167-->void{x166}
 - Intent{x143}.setTarget <: (String{x164})--e167-->void{x166}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x143})--e169-->void{x168}
 - Main{x127}.startActivity <: (Intent{x143})--e169-->void{x168}
 - { o174 } <= x172
 - ()--e191-->Intent{x185} <: ()--e173-->Intent{x172}
 - Intent{x172}.Intent <: ()--e173-->Intent{x172}
 - Intent{x172} <: Intent{x144}
 - { o176 } <= x175
 - (String{x194})--e196-->void{x195} <: (String{x175})--e178-->void{x177}
 - Intent{x144}.setTarget <: (String{x175})--e178-->void{x177}
 - { o180 } <= x179
 - (Object{x198})--e200-->void{x199} <: (String{x179})--e182-->void{x181}
 - Intent{x144}.setData <: (String{x179})--e182-->void{x181}
 - (Intent{x121})--e123-->void{x122} <: (Intent{x144})--e184-->void{x183}
 - Main{x127}.startActivity <: (Intent{x144})--e184-->void{x183}
 - {} U {} U {} U {} U e151 U {} U {} U {} U {} U e156 U {} U {} U {} U e158 U {} U {} U {} U {} U e162 U {} U {} U {} U {} U e167 U {} U {} U {} U e169 U {} U {} U {} U {} U e173 U {} U {} U {} U {} U e178 U {} U {} U {} U e182 U {} U {} U {} U e184 U {} <= e147
 - ()--e132-->void{x131} <: ()--e112-->void{x111}
 - (int{x145})--e147-->void{x146} <: (int{x113})--e115-->void{x114}
 - null{x187} <: String{x188}
 - null{x187} <: Intent{x186}.target
 - null{x189} <: Object{x190}
 - null{x189} <: Intent{x186}.data
 - { o174 } <= x185
 - Object{x192}.Object <: ()--e193-->Object{x192}
 - { o174 } <= x192
 - e193 U {} <= e191
 - String{x194} <: String{x188}
 - String{x194} <: Intent{x186}.target
 - {  } <= x197
 - {} U {} <= e196
 - Object{x198} <: Object{x190}
 - Object{x198} <: Intent{x186}.data
 - {  } <= x201
 - {} U {} <= e200
 - { o174 } <= x186
 - Object{x190} <: Object{x204}
 - Intent{x186}.data <: Object{x204}
 - Object{x204} <: Object{x202}
 - {} <= e203
 - null{x207} <: String{x208}
 - null{x207} <: Intent{x206}.target
 - null{x209} <: Object{x210}
 - null{x209} <: Intent{x206}.data
 - { o163 } <= x205
 - Object{x212}.Object <: ()--e213-->Object{x212}
 - { o163 } <= x212
 - e213 U {} <= e211
 - String{x214} <: String{x208}
 - String{x214} <: Intent{x206}.target
 - {  } <= x217
 - {} U {} <= e216
 - Object{x218} <: Object{x210}
 - Object{x218} <: Intent{x206}.data
 - {  } <= x221
 - {} U {} <= e220
 - { o163 } <= x206
 - Object{x210} <: Object{x224}
 - Intent{x206}.data <: Object{x224}
 - Object{x224} <: Object{x222}
 - {} <= e223
 - null{x227} <: String{x228}
 - null{x227} <: Intent{x226}.target
 - null{x229} <: Object{x230}
 - null{x229} <: Intent{x226}.data
 - { o152 } <= x225
 - Object{x232}.Object <: ()--e233-->Object{x232}
 - { o152 } <= x232
 - e233 U {} <= e231
 - String{x234} <: String{x228}
 - String{x234} <: Intent{x226}.target
 - {  } <= x237
 - {} U {} <= e236
 - Object{x238} <: Object{x230}
 - Object{x238} <: Intent{x226}.data
 - {  } <= x241
 - {} U {} <= e240
 - { o152 } <= x226
 - Object{x230} <: Object{x244}
 - Intent{x226}.data <: Object{x244}
 - Object{x244} <: Object{x242}
 - {} <= e243
 - null{x253} <: Intent{x254}
 - null{x253} <: Activity{x252}.intent
 - { o249 } <= x251
 - Object{x256}.Object <: ()--e257-->Object{x256}
 - { o249 } <= x256
 - e257 U {} <= e255
 - Intent{x254} <: Intent{x260}
 - Activity{x252}.intent <: Intent{x260}
 - Intent{x260} <: Intent{x258}
 - {} <= e259
 - {} <= e262
 - {} <= e265
 - {} U {} U e269 <= e268
 - { o249 } <= x252
 - {} <= e274
 - {Main} <= e274
 - Intent{x271} <: Intent{x355}
 - Intent{x271} <: Intent{x511}
 - {Main,Game} <= e274
 - Intent{x271} <: Game{x246}.intent
 - Intent{x271} <: Main{x507}.intent
 - Intent{x271} ~~~> e274 @ [{o249},ActivitystartActivity,7,1, n/a]
 - {} U {} U e274 <= e273
 - { o249 } <= x276
 - ()--e255-->Activity{x251} <: ()--e280-->Activity{x279}
 - Activity{x279}.Activity <: ()--e280-->Activity{x279}
 - { o249 } <= x279
 - e280 U {} <= e278
 - (int{x266})--e268-->void{x267} <: (int{x283})--e285-->void{x284}
 - Help{x277}.addButton <: (int{x283})--e285-->void{x284}
 - {} U {} U {} U e285 <= e282
 - { o249 } <= x277
 - { o295 } <= x293
 - ()--e492-->Intent{x486} <: ()--e294-->Intent{x293}
 - Intent{x293}.Intent <: ()--e294-->Intent{x293}
 - Intent{x293} <: Intent{x286}
 - ()--e259-->Intent{x258} <: ()--e297-->Intent{x296}
 - Help{x277}.getIntent <: ()--e297-->Intent{x296}
 - Intent{x296} <: Intent{x287}
 - ()--e444-->Object{x443} <: ()--e299-->Object{x298}
 - ()--e464-->Object{x463} <: ()--e299-->Object{x298}
 - ()--e203-->Object{x202} <: ()--e299-->Object{x298}
 - ()--e223-->Object{x222} <: ()--e299-->Object{x298}
 - ()--e243-->Object{x242} <: ()--e299-->Object{x298}
 - Intent{x287}.getData <: ()--e299-->Object{x298}
 - Object{x298} <: Object{x288}
 - String{x288} <: String{x289}
 - (String{x495})--e497-->void{x496} <: (String{x289})--e301-->void{x300}
 - Intent{x286}.setTarget <: (String{x289})--e301-->void{x300}
 - (Intent{x271})--e273-->void{x272} <: (Intent{x286})--e303-->void{x302}
 - Help{x277}.startActivity <: (Intent{x286})--e303-->void{x302}
 - {} U e294 U {} U {} U {} U e297 U {} U {} U e299 U {} U {} U {} U {} U e301 U {} U {} U {} U e303 <= e292
 - null{x306} <: Intent{x307}
 - null{x306} <: Activity{x305}.intent
 - { o247 } <= x304
 - Object{x309}.Object <: ()--e310-->Object{x309}
 - { o247 } <= x309
 - e310 U {} <= e308
 - Intent{x307} <: Intent{x313}
 - Activity{x305}.intent <: Intent{x313}
 - Intent{x313} <: Intent{x311}
 - {} <= e312
 - {} <= e315
 - {} <= e318
 - {} U {} U e322 <= e321
 - { o247 } <= x305
 - {} <= e327
 - Intent{x324} <: Intent{x511}
 - {Main} <= e327
 - Intent{x324} <: Main{x507}.intent
 - { o506 } <= x507
 - Intent{x324} ~~~> e327 @ [{o247},ActivitystartActivity,7,1, n/a]
 - {} U {} U e327 <= e326
 - { o247 } <= x329
 - ()--e308-->Activity{x304} <: ()--e333-->Activity{x332}
 - Activity{x332}.Activity <: ()--e333-->Activity{x332}
 - { o247 } <= x332
 - e333 U {} <= e331
 - (int{x319})--e321-->void{x320} <: (int{x336})--e338-->void{x337}
 - Score{x330}.addButton <: (int{x336})--e338-->void{x337}
 - {} U {} U {} U e338 <= e335
 - { o247 } <= x330
 - { o345 } <= x343
 - ()--e472-->Intent{x466} <: ()--e344-->Intent{x343}
 - Intent{x343}.Intent <: ()--e344-->Intent{x343}
 - Intent{x343} <: Intent{x339}
 - { o347 } <= x346
 - (String{x475})--e477-->void{x476} <: (String{x346})--e349-->void{x348}
 - Intent{x339}.setTarget <: (String{x346})--e349-->void{x348}
 - (Intent{x324})--e326-->void{x325} <: (Intent{x339})--e351-->void{x350}
 - Score{x330}.startActivity <: (Intent{x339})--e351-->void{x350}
 - {} U e344 U {} U {} U {} U {} U e349 U {} U {} U {} U e351 <= e342
 - null{x354} <: Intent{x355}
 - null{x354} <: Activity{x353}.intent
 - { o245 } <= x352
 - Object{x357}.Object <: ()--e358-->Object{x357}
 - { o245 } <= x357
 - e358 U {} <= e356
 - Intent{x355} <: Intent{x361}
 - Activity{x353}.intent <: Intent{x361}
 - Intent{x361} <: Intent{x359}
 - {} <= e360
 - {} <= e363
 - {} <= e366
 - {} U {} U e370 <= e369
 - { o245 } <= x353
 - {} <= e375
 - Intent{x372} <: Intent{x307}
 - Intent{x372} <: Intent{x254}
 - {Help,Score} <= e375
 - Intent{x372} <: Score{x248}.intent
 - Intent{x372} <: Help{x250}.intent
 - Intent{x372} ~~~> e375 @ [{o245},ActivitystartActivity,7,1, n/a]
 - {} U {} U e375 <= e374
 - { o245 } <= x377
 - ()--e356-->Activity{x352} <: ()--e381-->Activity{x380}
 - Activity{x380}.Activity <: ()--e381-->Activity{x380}
 - { o245 } <= x380
 - e381 U {} <= e379
 - (int{x367})--e369-->void{x368} <: (int{x384})--e386-->void{x385}
 - Game{x378}.addButton <: (int{x384})--e386-->void{x385}
 - (int{x367})--e369-->void{x368} <: (int{x387})--e389-->void{x388}
 - Game{x378}.addButton <: (int{x387})--e389-->void{x388}
 - (int{x367})--e369-->void{x368} <: (int{x390})--e392-->void{x391}
 - Game{x378}.addButton <: (int{x390})--e392-->void{x391}
 - {} U {} U {} U e386 U {} U {} U {} U e389 U {} U {} U {} U e392 <= e383
 - { o245 } <= x378
 - { o402 } <= x400
 - ()--e452-->Intent{x446} <: ()--e401-->Intent{x400}
 - Intent{x400}.Intent <: ()--e401-->Intent{x400}
 - Intent{x400} <: Intent{x393}
 - { o404 } <= x403
 - (String{x455})--e457-->void{x456} <: (String{x403})--e406-->void{x405}
 - Intent{x393}.setTarget <: (String{x403})--e406-->void{x405}
 - { o408 } <= x407
 - (Object{x459})--e461-->void{x460} <: (String{x407})--e410-->void{x409}
 - Intent{x393}.setData <: (String{x407})--e410-->void{x409}
 - (Intent{x372})--e374-->void{x373} <: (Intent{x393})--e412-->void{x411}
 - Game{x378}.startActivity <: (Intent{x393})--e412-->void{x411}
 - { o417 } <= x415
 - ()--e432-->Intent{x426} <: ()--e416-->Intent{x415}
 - Intent{x415}.Intent <: ()--e416-->Intent{x415}
 - Intent{x415} <: Intent{x394}
 - { o419 } <= x418
 - (String{x435})--e437-->void{x436} <: (String{x418})--e421-->void{x420}
 - Intent{x394}.setTarget <: (String{x418})--e421-->void{x420}
 - (Intent{x372})--e374-->void{x373} <: (Intent{x394})--e423-->void{x422}
 - Game{x378}.startActivity <: (Intent{x394})--e423-->void{x422}
 - {} U {} U {} U {} U e401 U {} U {} U {} U {} U e406 U {} U {} U {} U e410 U {} U {} U {} U e412 U {} U {} U {} U {} U e416 U {} U {} U {} U {} U e421 U {} U {} U {} U e423 U {} U {} U {} U {} U {} <= e397
 - null{x428} <: String{x429}
 - null{x428} <: Intent{x427}.target
 - null{x430} <: Object{x431}
 - null{x430} <: Intent{x427}.data
 - { o417 } <= x426
 - Object{x433}.Object <: ()--e434-->Object{x433}
 - { o417 } <= x433
 - e434 U {} <= e432
 - String{x435} <: String{x429}
 - String{x435} <: Intent{x427}.target
 - {  } <= x438
 - {} U {} <= e437
 - Object{x439} <: Object{x431}
 - Object{x439} <: Intent{x427}.data
 - {  } <= x442
 - {} U {} <= e441
 - { o417 } <= x427
 - Object{x431} <: Object{x445}
 - Intent{x427}.data <: Object{x445}
 - Object{x445} <: Object{x443}
 - {} <= e444
 - null{x448} <: String{x449}
 - null{x448} <: Intent{x447}.target
 - null{x450} <: Object{x451}
 - null{x450} <: Intent{x447}.data
 - { o402 } <= x446
 - Object{x453}.Object <: ()--e454-->Object{x453}
 - { o402 } <= x453
 - e454 U {} <= e452
 - String{x455} <: String{x449}
 - String{x455} <: Intent{x447}.target
 - {  } <= x458
 - {} U {} <= e457
 - Object{x459} <: Object{x451}
 - Object{x459} <: Intent{x447}.data
 - {  } <= x462
 - {} U {} <= e461
 - { o402 } <= x447
 - Object{x451} <: Object{x465}
 - Intent{x447}.data <: Object{x465}
 - Object{x465} <: Object{x463}
 - {} <= e464
 - null{x468} <: String{x469}
 - null{x468} <: Intent{x467}.target
 - null{x470} <: Object{x471}
 - null{x470} <: Intent{x467}.data
 - { o345 } <= x466
 - Object{x473}.Object <: ()--e474-->Object{x473}
 - { o345 } <= x473
 - e474 U {} <= e472
 - String{x475} <: String{x469}
 - String{x475} <: Intent{x467}.target
 - {  } <= x478
 - {} U {} <= e477
 - Object{x479} <: Object{x471}
 - Object{x479} <: Intent{x467}.data
 - {  } <= x482
 - {} U {} <= e481
 - { o345 } <= x467
 - Object{x471} <: Object{x485}
 - Intent{x467}.data <: Object{x485}
 - Object{x485} <: Object{x483}
 - {} <= e484
 - null{x488} <: String{x489}
 - null{x488} <: Intent{x487}.target
 - null{x490} <: Object{x491}
 - null{x490} <: Intent{x487}.data
 - { o295 } <= x486
 - Object{x493}.Object <: ()--e494-->Object{x493}
 - { o295 } <= x493
 - e494 U {} <= e492
 - String{x495} <: String{x489}
 - String{x495} <: Intent{x487}.target
 - {  } <= x498
 - {} U {} <= e497
 - Object{x499} <: Object{x491}
 - Object{x499} <: Intent{x487}.data
 - {  } <= x502
 - {} U {} <= e501
 - { o295 } <= x487
 - Object{x491} <: Object{x505}
 - Intent{x487}.data <: Object{x505}
 - Object{x505} <: Object{x503}
 - {} <= e504
 - null{x510} <: Intent{x511}
 - null{x510} <: Activity{x509}.intent
 - { o506 } <= x508
 - Object{x513}.Object <: ()--e514-->Object{x513}
 - { o506 } <= x513
 - e514 U {} <= e512
 - Intent{x511} <: Intent{x517}
 - Activity{x509}.intent <: Intent{x517}
 - Intent{x517} <: Intent{x515}
 - {} <= e516
 - {} <= e519
 - {} <= e522
 - {} U {} U e526 <= e525
 - { o506 } <= x509
 - {} <= e531
 - Intent{x528} <: Intent{x254}
 - Intent{x528} <: Intent{x307}
 - Intent{x528} <: Intent{x355}
 - {Game,Score,Help} <= e531
 - Intent{x528} <: Help{x250}.intent
 - { o249 } <= x250
 - Intent{x528} <: Score{x248}.intent
 - { o247 } <= x248
 - Intent{x528} <: Game{x246}.intent
 - { o245 } <= x246
 - Intent{x528} ~~~> e531 @ [{o506},ActivitystartActivity,7,1, n/a]
 - {} U {} U e531 <= e530
 - { o506 } <= x533
 - ()--e512-->Activity{x508} <: ()--e537-->Activity{x536}
 - Activity{x536}.Activity <: ()--e537-->Activity{x536}
 - { o506 } <= x536
 - e537 U {} <= e535
 - (int{x523})--e525-->void{x524} <: (int{x540})--e542-->void{x541}
 - Main{x534}.addButton <: (int{x540})--e542-->void{x541}
 - (int{x523})--e525-->void{x524} <: (int{x543})--e545-->void{x544}
 - Main{x534}.addButton <: (int{x543})--e545-->void{x544}
 - (int{x523})--e525-->void{x524} <: (int{x546})--e548-->void{x547}
 - Main{x534}.addButton <: (int{x546})--e548-->void{x547}
 - {} U {} U {} U e542 U {} U {} U {} U e545 U {} U {} U {} U e548 <= e539
 - { o506 } <= x534
 - { o152 } <= x557
 - ()--e231-->Intent{x225} <: ()--e558-->Intent{x557}
 - Intent{x557}.Intent <: ()--e558-->Intent{x557}
 - Intent{x557} <: Intent{x549}
 - { o154 } <= x559
 - (String{x234})--e236-->void{x235} <: (String{x559})--e561-->void{x560}
 - Intent{x549}.setTarget <: (String{x559})--e561-->void{x560}
 - (Intent{x528})--e530-->void{x529} <: (Intent{x549})--e563-->void{x562}
 - Main{x534}.startActivity <: (Intent{x549})--e563-->void{x562}
 - { o163 } <= x566
 - ()--e211-->Intent{x205} <: ()--e567-->Intent{x566}
 - Intent{x566}.Intent <: ()--e567-->Intent{x566}
 - Intent{x566} <: Intent{x550}
 - { o165 } <= x568
 - (String{x214})--e216-->void{x215} <: (String{x568})--e570-->void{x569}
 - Intent{x550}.setTarget <: (String{x568})--e570-->void{x569}
 - (Intent{x528})--e530-->void{x529} <: (Intent{x550})--e572-->void{x571}
 - Main{x534}.startActivity <: (Intent{x550})--e572-->void{x571}
 - { o174 } <= x575
 - ()--e191-->Intent{x185} <: ()--e576-->Intent{x575}
 - Intent{x575}.Intent <: ()--e576-->Intent{x575}
 - Intent{x575} <: Intent{x551}
 - { o176 } <= x577
 - (String{x194})--e196-->void{x195} <: (String{x577})--e579-->void{x578}
 - Intent{x551}.setTarget <: (String{x577})--e579-->void{x578}
 - { o180 } <= x580
 - (Object{x198})--e200-->void{x199} <: (String{x580})--e582-->void{x581}
 - Intent{x551}.setData <: (String{x580})--e582-->void{x581}
 - (Intent{x528})--e530-->void{x529} <: (Intent{x551})--e584-->void{x583}
 - Main{x534}.startActivity <: (Intent{x551})--e584-->void{x583}
 - {} U {} U {} U {} U e558 U {} U {} U {} U {} U e561 U {} U {} U {} U e563 U {} U {} U {} U {} U e567 U {} U {} U {} U {} U e570 U {} U {} U {} U e572 U {} U {} U {} U {} U e576 U {} U {} U {} U {} U e579 U {} U {} U {} U e582 U {} U {} U {} U e584 U {} <= e554
 - ()--e539-->void{x538} <: ()--e519-->void{x518}
 - (int{x552})--e554-->void{x553} <: (int{x520})--e522-->void{x521}
 - ()--e282-->void{x281} <: ()--e262-->void{x261}
 - (int{x290})--e292-->void{x291} <: (int{x263})--e265-->void{x264}
 - ()--e335-->void{x334} <: ()--e315-->void{x314}
 - (int{x340})--e342-->void{x341} <: (int{x316})--e318-->void{x317}
 - ()--e383-->void{x382} <: ()--e363-->void{x362}
 - (int{x395})--e397-->void{x396} <: (int{x364})--e366-->void{x365}
Typing table:
 - Activity{o1}.Activity,1,return,1 = Activity{x101}
 - Activity{o1},this,0 = Activity{x102}
 - Activity{o1}.intent,2 = Intent{x104}
 - Activity{o1}.Activity,1 = ()--e105-->Activity{x101}
 - Activity{o1}.getIntent,3,return,1 = Intent{x108}
 - Activity{o1}.getIntent,3 = ()--e109-->Intent{x108}
 - Activity{o1}.onCreate,4,return,1 = void{x111}
 - Activity{o1}.onCreate,4 = ()--e112-->void{x111}
 - Activity{o1}.onClick,5,button,2 = int{x113}
 - Activity{o1}.onClick,5,return,1 = void{x114}
 - Activity{o1}.onClick,5 = (int{x113})--e115-->void{x114}
 - Activity{o1}.addButton,6,button,2 = int{x116}
 - Activity{o1}.addButton,6,return,1 = void{x117}
 - Activity{o1}.addButton,6 = (int{x116})--e118-->void{x117}
 - Activity{o1}.startActivity,7,i,2 = Intent{x121}
 - Activity{o1}.startActivity,7,return,1 = void{x122}
 - Activity{o1}.startActivity,7 = (Intent{x121})--e123-->void{x122}
 - Main{o1}.Main,1,return,1 = Main{x126}
 - Main{o1},this,0 = Main{x127}
 - Main{o1}.Main,1 = ()--e128-->Main{x126}
 - Main{o1}.onCreate,2,return,1 = void{x131}
 - Main{o1}.onCreate,2 = ()--e132-->void{x131}
 - Main{o1}.onClick,3,i,3 = Intent{x142}
 - Main{o1}.onClick,3,i,4 = Intent{x143}
 - Main{o1}.onClick,3,i,5 = Intent{x144}
 - Main{o1}.onClick,3,button,2 = int{x145}
 - Main{o1}.onClick,3,return,1 = void{x146}
 - Main{o1}.onClick,3 = (int{x145})--e147-->void{x146}
 - Intent{o174}.Intent,1,return,1 = Intent{x185}
 - Intent{o174},this,0 = Intent{x186}
 - Intent{o174}.target,2 = String{x188}
 - Intent{o174}.data,3 = Object{x190}
 - Intent{o174}.Intent,1 = ()--e191-->Intent{x185}
 - Intent{o174}.setTarget,4,s,2 = String{x194}
 - Intent{o174}.setTarget,4,return,1 = void{x195}
 - Intent{o174}.setTarget,4 = (String{x194})--e196-->void{x195}
 - Intent{o174}.setData,5,d,2 = Object{x198}
 - Intent{o174}.setData,5,return,1 = void{x199}
 - Intent{o174}.setData,5 = (Object{x198})--e200-->void{x199}
 - Intent{o174}.getData,6,return,1 = Object{x202}
 - Intent{o174}.getData,6 = ()--e203-->Object{x202}
 - Intent{o163}.Intent,1,return,1 = Intent{x205}
 - Intent{o163},this,0 = Intent{x206}
 - Intent{o163}.target,2 = String{x208}
 - Intent{o163}.data,3 = Object{x210}
 - Intent{o163}.Intent,1 = ()--e211-->Intent{x205}
 - Intent{o163}.setTarget,4,s,2 = String{x214}
 - Intent{o163}.setTarget,4,return,1 = void{x215}
 - Intent{o163}.setTarget,4 = (String{x214})--e216-->void{x215}
 - Intent{o163}.setData,5,d,2 = Object{x218}
 - Intent{o163}.setData,5,return,1 = void{x219}
 - Intent{o163}.setData,5 = (Object{x218})--e220-->void{x219}
 - Intent{o163}.getData,6,return,1 = Object{x222}
 - Intent{o163}.getData,6 = ()--e223-->Object{x222}
 - Intent{o152}.Intent,1,return,1 = Intent{x225}
 - Intent{o152},this,0 = Intent{x226}
 - Intent{o152}.target,2 = String{x228}
 - Intent{o152}.data,3 = Object{x230}
 - Intent{o152}.Intent,1 = ()--e231-->Intent{x225}
 - Intent{o152}.setTarget,4,s,2 = String{x234}
 - Intent{o152}.setTarget,4,return,1 = void{x235}
 - Intent{o152}.setTarget,4 = (String{x234})--e236-->void{x235}
 - Intent{o152}.setData,5,d,2 = Object{x238}
 - Intent{o152}.setData,5,return,1 = void{x239}
 - Intent{o152}.setData,5 = (Object{x238})--e240-->void{x239}
 - Intent{o152}.getData,6,return,1 = Object{x242}
 - Intent{o152}.getData,6 = ()--e243-->Object{x242}
 - Activity{o249}.Activity,1,return,1 = Activity{x251}
 - Activity{o249},this,0 = Activity{x252}
 - Activity{o249}.intent,2 = Intent{x254}
 - Activity{o249}.Activity,1 = ()--e255-->Activity{x251}
 - Activity{o249}.getIntent,3,return,1 = Intent{x258}
 - Activity{o249}.getIntent,3 = ()--e259-->Intent{x258}
 - Activity{o249}.onCreate,4,return,1 = void{x261}
 - Activity{o249}.onCreate,4 = ()--e262-->void{x261}
 - Activity{o249}.onClick,5,button,2 = int{x263}
 - Activity{o249}.onClick,5,return,1 = void{x264}
 - Activity{o249}.onClick,5 = (int{x263})--e265-->void{x264}
 - Activity{o249}.addButton,6,button,2 = int{x266}
 - Activity{o249}.addButton,6,return,1 = void{x267}
 - Activity{o249}.addButton,6 = (int{x266})--e268-->void{x267}
 - Activity{o249}.startActivity,7,i,2 = Intent{x271}
 - Activity{o249}.startActivity,7,return,1 = void{x272}
 - Activity{o249}.startActivity,7 = (Intent{x271})--e273-->void{x272}
 - Help{o249}.Help,1,return,1 = Help{x276}
 - Help{o249},this,0 = Help{x277}
 - Help{o249}.Help,1 = ()--e278-->Help{x276}
 - Help{o249}.onCreate,2,return,1 = void{x281}
 - Help{o249}.onCreate,2 = ()--e282-->void{x281}
 - Help{o249}.onClick,3,i,3 = Intent{x286}
 - Help{o249}.onClick,3,j,4 = Intent{x287}
 - Help{o249}.onClick,3,o,5 = Object{x288}
 - Help{o249}.onClick,3,t,6 = String{x289}
 - Help{o249}.onClick,3,button,2 = int{x290}
 - Help{o249}.onClick,3,return,1 = void{x291}
 - Help{o249}.onClick,3 = (int{x290})--e292-->void{x291}
 - Activity{o247}.Activity,1,return,1 = Activity{x304}
 - Activity{o247},this,0 = Activity{x305}
 - Activity{o247}.intent,2 = Intent{x307}
 - Activity{o247}.Activity,1 = ()--e308-->Activity{x304}
 - Activity{o247}.getIntent,3,return,1 = Intent{x311}
 - Activity{o247}.getIntent,3 = ()--e312-->Intent{x311}
 - Activity{o247}.onCreate,4,return,1 = void{x314}
 - Activity{o247}.onCreate,4 = ()--e315-->void{x314}
 - Activity{o247}.onClick,5,button,2 = int{x316}
 - Activity{o247}.onClick,5,return,1 = void{x317}
 - Activity{o247}.onClick,5 = (int{x316})--e318-->void{x317}
 - Activity{o247}.addButton,6,button,2 = int{x319}
 - Activity{o247}.addButton,6,return,1 = void{x320}
 - Activity{o247}.addButton,6 = (int{x319})--e321-->void{x320}
 - Activity{o247}.startActivity,7,i,2 = Intent{x324}
 - Activity{o247}.startActivity,7,return,1 = void{x325}
 - Activity{o247}.startActivity,7 = (Intent{x324})--e326-->void{x325}
 - Score{o247}.Score,1,return,1 = Score{x329}
 - Score{o247},this,0 = Score{x330}
 - Score{o247}.Score,1 = ()--e331-->Score{x329}
 - Score{o247}.onCreate,2,return,1 = void{x334}
 - Score{o247}.onCreate,2 = ()--e335-->void{x334}
 - Score{o247}.onClick,3,i,3 = Intent{x339}
 - Score{o247}.onClick,3,button,2 = int{x340}
 - Score{o247}.onClick,3,return,1 = void{x341}
 - Score{o247}.onClick,3 = (int{x340})--e342-->void{x341}
 - Activity{o245}.Activity,1,return,1 = Activity{x352}
 - Activity{o245},this,0 = Activity{x353}
 - Activity{o245}.intent,2 = Intent{x355}
 - Activity{o245}.Activity,1 = ()--e356-->Activity{x352}
 - Activity{o245}.getIntent,3,return,1 = Intent{x359}
 - Activity{o245}.getIntent,3 = ()--e360-->Intent{x359}
 - Activity{o245}.onCreate,4,return,1 = void{x362}
 - Activity{o245}.onCreate,4 = ()--e363-->void{x362}
 - Activity{o245}.onClick,5,button,2 = int{x364}
 - Activity{o245}.onClick,5,return,1 = void{x365}
 - Activity{o245}.onClick,5 = (int{x364})--e366-->void{x365}
 - Activity{o245}.addButton,6,button,2 = int{x367}
 - Activity{o245}.addButton,6,return,1 = void{x368}
 - Activity{o245}.addButton,6 = (int{x367})--e369-->void{x368}
 - Activity{o245}.startActivity,7,i,2 = Intent{x372}
 - Activity{o245}.startActivity,7,return,1 = void{x373}
 - Activity{o245}.startActivity,7 = (Intent{x372})--e374-->void{x373}
 - Game{o245}.Game,1,return,1 = Game{x377}
 - Game{o245},this,0 = Game{x378}
 - Game{o245}.Game,1 = ()--e379-->Game{x377}
 - Game{o245}.onCreate,2,return,1 = void{x382}
 - Game{o245}.onCreate,2 = ()--e383-->void{x382}
 - Game{o245}.onClick,3,i,3 = Intent{x393}
 - Game{o245}.onClick,3,i,4 = Intent{x394}
 - Game{o245}.onClick,3,button,2 = int{x395}
 - Game{o245}.onClick,3,return,1 = void{x396}
 - Game{o245}.onClick,3 = (int{x395})--e397-->void{x396}
 - Intent{o417}.Intent,1,return,1 = Intent{x426}
 - Intent{o417},this,0 = Intent{x427}
 - Intent{o417}.target,2 = String{x429}
 - Intent{o417}.data,3 = Object{x431}
 - Intent{o417}.Intent,1 = ()--e432-->Intent{x426}
 - Intent{o417}.setTarget,4,s,2 = String{x435}
 - Intent{o417}.setTarget,4,return,1 = void{x436}
 - Intent{o417}.setTarget,4 = (String{x435})--e437-->void{x436}
 - Intent{o417}.setData,5,d,2 = Object{x439}
 - Intent{o417}.setData,5,return,1 = void{x440}
 - Intent{o417}.setData,5 = (Object{x439})--e441-->void{x440}
 - Intent{o417}.getData,6,return,1 = Object{x443}
 - Intent{o417}.getData,6 = ()--e444-->Object{x443}
 - Intent{o402}.Intent,1,return,1 = Intent{x446}
 - Intent{o402},this,0 = Intent{x447}
 - Intent{o402}.target,2 = String{x449}
 - Intent{o402}.data,3 = Object{x451}
 - Intent{o402}.Intent,1 = ()--e452-->Intent{x446}
 - Intent{o402}.setTarget,4,s,2 = String{x455}
 - Intent{o402}.setTarget,4,return,1 = void{x456}
 - Intent{o402}.setTarget,4 = (String{x455})--e457-->void{x456}
 - Intent{o402}.setData,5,d,2 = Object{x459}
 - Intent{o402}.setData,5,return,1 = void{x460}
 - Intent{o402}.setData,5 = (Object{x459})--e461-->void{x460}
 - Intent{o402}.getData,6,return,1 = Object{x463}
 - Intent{o402}.getData,6 = ()--e464-->Object{x463}
 - Intent{o345}.Intent,1,return,1 = Intent{x466}
 - Intent{o345},this,0 = Intent{x467}
 - Intent{o345}.target,2 = String{x469}
 - Intent{o345}.data,3 = Object{x471}
 - Intent{o345}.Intent,1 = ()--e472-->Intent{x466}
 - Intent{o345}.setTarget,4,s,2 = String{x475}
 - Intent{o345}.setTarget,4,return,1 = void{x476}
 - Intent{o345}.setTarget,4 = (String{x475})--e477-->void{x476}
 - Intent{o345}.setData,5,d,2 = Object{x479}
 - Intent{o345}.setData,5,return,1 = void{x480}
 - Intent{o345}.setData,5 = (Object{x479})--e481-->void{x480}
 - Intent{o345}.getData,6,return,1 = Object{x483}
 - Intent{o345}.getData,6 = ()--e484-->Object{x483}
 - Intent{o295}.Intent,1,return,1 = Intent{x486}
 - Intent{o295},this,0 = Intent{x487}
 - Intent{o295}.target,2 = String{x489}
 - Intent{o295}.data,3 = Object{x491}
 - Intent{o295}.Intent,1 = ()--e492-->Intent{x486}
 - Intent{o295}.setTarget,4,s,2 = String{x495}
 - Intent{o295}.setTarget,4,return,1 = void{x496}
 - Intent{o295}.setTarget,4 = (String{x495})--e497-->void{x496}
 - Intent{o295}.setData,5,d,2 = Object{x499}
 - Intent{o295}.setData,5,return,1 = void{x500}
 - Intent{o295}.setData,5 = (Object{x499})--e501-->void{x500}
 - Intent{o295}.getData,6,return,1 = Object{x503}
 - Intent{o295}.getData,6 = ()--e504-->Object{x503}
 - Activity{o506}.Activity,1,return,1 = Activity{x508}
 - Activity{o506},this,0 = Activity{x509}
 - Activity{o506}.intent,2 = Intent{x511}
 - Activity{o506}.Activity,1 = ()--e512-->Activity{x508}
 - Activity{o506}.getIntent,3,return,1 = Intent{x515}
 - Activity{o506}.getIntent,3 = ()--e516-->Intent{x515}
 - Activity{o506}.onCreate,4,return,1 = void{x518}
 - Activity{o506}.onCreate,4 = ()--e519-->void{x518}
 - Activity{o506}.onClick,5,button,2 = int{x520}
 - Activity{o506}.onClick,5,return,1 = void{x521}
 - Activity{o506}.onClick,5 = (int{x520})--e522-->void{x521}
 - Activity{o506}.addButton,6,button,2 = int{x523}
 - Activity{o506}.addButton,6,return,1 = void{x524}
 - Activity{o506}.addButton,6 = (int{x523})--e525-->void{x524}
 - Activity{o506}.startActivity,7,i,2 = Intent{x528}
 - Activity{o506}.startActivity,7,return,1 = void{x529}
 - Activity{o506}.startActivity,7 = (Intent{x528})--e530-->void{x529}
 - Main{o506}.Main,1,return,1 = Main{x533}
 - Main{o506},this,0 = Main{x534}
 - Main{o506}.Main,1 = ()--e535-->Main{x533}
 - Main{o506}.onCreate,2,return,1 = void{x538}
 - Main{o506}.onCreate,2 = ()--e539-->void{x538}
 - Main{o506}.onClick,3,i,3 = Intent{x549}
 - Main{o506}.onClick,3,i,4 = Intent{x550}
 - Main{o506}.onClick,3,i,5 = Intent{x551}
 - Main{o506}.onClick,3,button,2 = int{x552}
 - Main{o506}.onClick,3,return,1 = void{x553}
 - Main{o506}.onClick,3 = (int{x552})--e554-->void{x553}

Allocated Objects and Their Labels
 - o1 : (Android Platform,launch,1,0,n/a) new Main 
 - o152 : (Main,onClick,3,1,n/a) new Intent 
 - o154 : (String,lit_Game,0,2,n/a) new String "Game"
 - o163 : (Main,onClick,3,3,n/a) new Intent 
 - o165 : (String,lit_Score,0,4,n/a) new String "Score"
 - o174 : (Main,onClick,3,5,n/a) new Intent 
 - o176 : (String,lit_Help,0,6,n/a) new String "Help"
 - o180 : (String,lit_Main,0,7,n/a) new String "Main"
 - o245 : (Activity,startActivity,7,1,Game) new Game 
 - o247 : (Activity,startActivity,7,1,Score) new Score 
 - o249 : (Activity,startActivity,7,1,Help) new Help 
 - o295 : (Help,onClick,3,1,n/a) new Intent 
 - o345 : (Score,onClick,3,1,n/a) new Intent 
 - o347 : (String,lit_Main,0,2,n/a) new String "Main"
 - o402 : (Game,onClick,3,1,n/a) new Intent 
 - o404 : (String,lit_Help,0,2,n/a) new String "Help"
 - o408 : (String,lit_Game,0,3,n/a) new String "Game"
 - o417 : (Game,onClick,3,4,n/a) new Intent 
 - o419 : (String,lit_Score,0,5,n/a) new String "Score"
 - o506 : (Activity,startActivity,7,1,Main) new Main 

Allocated Contexual Objects
 - o152 : Intent{x150}
 - o154 : String{x153}
 - o163 : Intent{x161}
 - o165 : String{x164}
 - o174 : Intent{x172}
 - o176 : String{x175}
 - o180 : String{x179}
 - o245 : Game{x246}
 - o247 : Score{x248}
 - o249 : Help{x250}
 - o295 : Intent{x293}
 - o345 : Intent{x343}
 - o347 : String{x346}
 - o402 : Intent{x400}
 - o404 : String{x403}
 - o408 : String{x407}
 - o417 : Intent{x415}
 - o419 : String{x418}
 - o506 : Main{x507}

18.03526 seconds in total

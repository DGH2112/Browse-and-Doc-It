(**

  This is a description.
  
  @Date    01/Aug/2008
  @Version 1.0
  @Author  David Hoyle
  
  @stopdocumentation
  
**)
Unit Hello;

Interface

(** First Uses Clause **)
Uses
  Windows, SysUtils;

(** Global Types **)
Type
  (** Integer Types **)
  TInteger = Integer;
  TInteger2 = Double;
  TShortInt = ShortInt;
  TSmallInt = SmallInt;
  TLongInt = LongInt;
  TByte = Byte;
  TWord = Word;
  TLongWord = LongWord;
  (** Real Types **)
  TReal48 = Real48;
  TSingle = Single;
  TDouble = Double;
  TComp = Comp;
  TCurrency = Currency;
  (** Enumerates **)
  TMyEnum = (meLow, meHigh);
  (** Sub Range **)
  TMyInts = 1..9;                     
  (** Class Ref **)
  TMyClassRef = Class Of TObject;   
  (** Variants **)
  TVariant = Variant;              
  (** String Types **)
  TString = String;
  TAnsiString = AnsiString;
  TWideString = WideString;
  TShortString = ShortString;
  TMyString = String[10];         
  (** Pointer Types **) 
  PMyObject = ^MyObject;          
  (** Array types **)
  TArray = Array Of Integer;
  TIndexedArray = Array[1..9] Of String;
  TMultiArray = Array[1..9, 1..2] of Double; 
  (** Set types **)
  TSet = Set Of Integers;
  TIntSet = Set Of TEnum;                   
  (** File types **)
  TFile1 = File Of TRecord;
  TFile2 = File Of Integer; 
  TRecord = Record
    (** Name **)
    FName : String;
    FIndex : Integer;
  End;                       
  //: variant record type
  TEmployee = record
    FirstName, LastName: string[40];
    BirthDate: TDate;
    Case Salaried: Boolean of
      True: (AnnualSalary: Currency);
      False: (HourlyWage: Currency);
   end;                      
   //: Procedure types
   TSimpleProc = Procedure(iFirst : Integer);
   TSimpleFunc = Function(iFirst  :Integer; strSecond :String) : Boolean;
   (** Object types **)
   TMyObj = Object
     FName : String;
     FInt : Integer;
     Procedure Hello(i : Integer);
   End;                                 
   TMyObject = Object(TMyOtherClass)
     //: A name field
     FName : String;
     FInt64 : Int64;
     Procedure Proc1(iFirst : String);
     Function DGH(x, y : Double) : Double;
   End;                               
   (** Class Types **)
   TMyClass = Class(TObject, IInterface)
   Private
     FName : String;
   Protected
     Function GetAsString(Const iIndex : Integer) : String; Override;
   Public
     Function Goodbye(i, j : Integer; bool  : Boolean) : String;
     Function Goodbye2(i, j : Integer; str : String) : String;
     Property Ident : String Read FIdent Write FIdent;
     (**
       This property returns sub items.
       @precon  None.
       @postcon Something.
       @param   iIndex as an Integer
       @return  a String
     **)
     Property Items[iIndex : Integer] : String Read GetItems;
   End;
   (** Interface Types **)
   IMyInterface = Interface(IUnknown)
     Function F(x : String) : Integer;
     Property Name : String Read FName;
   End;
                                          
ResourceString
  strText = 'Hello Dave!';
  iComma = 'www';
                                        
Const
  (** Some simple constants. **)
  iConstant = 1;
  dblConstant = 1.2;
  q = 'Hello';
  strConstant = 'Hello';
  iComma = 1;
                                          
Var
  i : Integer;
                                       
ThreadVar
  strHello : String;
         
  Procedure Hello;                              
  Procedure MyProc; StdCall;              
  Function Add(x, y : Double) : Double;
                                       
Exports
  MyProc Name AMyProc,
  MyFunc Name 'MyFunction';

  Procedure OutputMessage(strText : String); Overload;
  Procedure OutputMessage(strFileName, strText, strPrefix : String; iLine, iCol : Integer); Overload;

Implementation

(** Second Uses Clause **)
Uses
  Classes;
  
Type
  (** Integer Types **)
  TCardinal = Cardinal;
  TInt64 = Int64;
  (** Real Types **)
  TExtended = Extended;
  (** Variants **)
  TOLEVariant = OLEVariant;
  TMyRecord = Record
    FOoops : String;
  End;
  TDGHObject = Object
    Procedure Hello;
  End;
  TOopsClass = Class
    FName : String;
  Protected
    Function Goodbye : Integer;
  End;
  IMyInt = Interface(IUnknown, ISomething)
    Procedure Goodbye;
  End;
  IMyDispInt = DispInterface(ISomething)
    Procedure Goodbye;
  End;

Var
  j : Double;
  
Procedure Hello;

  (**
    @precon None.
  **)
  Function X : Integer;
  
  Begin
  End;

Begin
  For i := 1 to 10 Do
    Begin
    
    End;
End;

(**
  
  This method returnss the string addition of i and j.
  
  @precon  None.
  @postcon Something.
  
  @param   i as an Integer
  @param   j as an Integer
  @param   bool as a Boolean
  @return  a String
  
**)
Function TMyClass.Goodbye(i, j : Integer; bool  : Boolean) : String;

Type
  Int = Integer;
  C = Class;

Const
  s = 'Hello';

Var
  k,l : Integer;
  
ResourceString
  q = 'Goodbye';
  
  Function W(i, j : Int) : Int;
  
  Const
    q = 1;
  
  Begin
  End;

Begin
  MyFunction(MyPointer^);
  MyFunction(P^, Nil^);
End;

Function TMyClass.Goodbye2(i, j : Integer; str : String) : String;

Begin
End;

Initialization
  DoSomething;
(** Goodbye **)
Finalization
  Dosomemore;
End.
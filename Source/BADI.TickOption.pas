(**

  This module contains a class to hold tick count information.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.TickOption;

Interface

Type
  (** A class to hold a 64 bit tick count against a name. **)
  TTickOption = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FName: String;
    FTickCount: Double;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(const strName: String; dblTickCount: Double);
    (**
      This property returns the name of the tick counter.
      @precon  None.
      @postcon Returns the name of the tick counter.
      @return  a String
    **)
    Property Name: String Read FName;
    (**
      This property returns the value of the tick counter.
      @precon  None.
      @postcon Returns the value of the tick counter.
      @return  a Double
    **)
    Property TickCount: Double Read FTickCount;
  End;

Implementation

(**

  A constructor for the TTickOption class.

  @precon  None.
  @postcon Initialises the tick counter.

  @param   strName      as a String as a Constant
  @param   dblTickCount as a Double

**)
Constructor TTickOption.Create(const strName: String; dblTickCount: Double);

Begin
  FName := strName;
  FTickCount := dblTickCount;
End;

End.

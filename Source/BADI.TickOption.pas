(**

  This module contains a class to hold tick count information.

  @Author  David Hoyle
  @Version 1.044
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    Constructor Create(Const strName: String; Const dblTickCount: Double);
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

  @param   strName      as a String as a constant
  @param   dblTickCount as a Double as a constant

**)
Constructor TTickOption.Create(Const strName: String; Const dblTickCount: Double);

Begin
  FName := strName;
  FTickCount := dblTickCount;
End;

End.

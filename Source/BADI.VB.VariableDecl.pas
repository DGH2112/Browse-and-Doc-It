(**

  This module contains a class to represent a VB variable declaration.

  @Author  Daivd Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.VB.VariableDecl;

Interface

Uses
  BADI.Generic.Variable,
  BADI.VB.Types,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** A class to represent variables in visual basic. **)
  TVBVar = Class(TGenericVariable)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDimensions, T : Array Of TArrayDimensions;
    FWithEvents : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetDimensions : Integer;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Destructor Destroy; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure AddDimension(Const strLow, strHigh : String);
    (**
      This property returns the number of dimensions in the array variable.
      @precon  None.
      @postcon Returns the number of dimensions in the array variable.
      @return  an Integer
    **)
    Property Dimensions : Integer Read GetDimensions;
    (**
      This property determines if the variable is an event interface.
      @precon  None.
      @postcon Determines if the variable is an event interface.
      @return  a Boolean
    **)
    Property WithEvents : Boolean Read FWithEvents Write FWithEvents;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options;

(**

  This method adds an array dimension to the varaiable declaration.

  @precon  None .
  @postcon Adds an array dimension to the varaiable declaration .

  @param   strLow  as a String as a Constant
  @param   strHigh as a String as a Constant

**)
procedure TVBVar.AddDimension(Const strLow, strHigh : String);

Var
  i : Integer;

begin
  T := Nil;
  If FDimensions = Nil Then
    Begin
      SetLength(FDimensions, 1);
      FDimensions[0][1] := strLow;
      FDimensions[0][2] := strHigh;
    End Else
    Begin
      T := Copy(FDimensions, 1, Length(FDimensions));
      SetLength(FDimensions, Succ(Succ(High(FDimensions))));
      For i := Low(T) To High(T) Do
        FDimensions[i] := T[i];
      FDimensions[High(FDimensions)][1] := strLow;
      FDimensions[High(FDimensions)][2] := strHigh;
    End;
end;

(**

  This method returns a string representation of the visual basic variable.

  @precon  None .
  @postcon Returns a string representation of the visual basic variable .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBVar.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Var
  i: Integer;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If WithEvents Then
    Result := 'WithEvents' + #32 + Result;
  If Dimensions > 0 Then
    Begin
      Result := Result + '(';
      For i := 0 To Dimensions - 1 Do
        Begin
          If i > 0 Then
            Result := Result + ', ';
          If FDimensions[i][1] <> '' Then
            Result := Result + Format('%s to %s', [FDimensions[i][1],
              FDimensions[i][2]]);
        End;
      Result := Result + ')';
    End;
  If TokenCount > 0 Then
    Result := Result + #32'As'#32 + BuildStringRepresentation(False,
      boolForDocumentation, '', BADIOptions.MaxDocOutputWidth);
end;

(**

  This is a constructor for the TVBVar class.

  @precon  None.
  @postcon Provides a reference for the variables array dimensions.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TVBVar.Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FDimensions := Nil;
End;

(**

  This is a destructor for the TVBVar class.

  @precon  None.
  @postcon Frees the memory for array dimensions.

**)
destructor TVBVar.Destroy;
begin
  FDimensions := Nil;
  Inherited Destroy;
end;

(**

  This is a getter method for the Dimensions property.

  @precon  None.
  @postcon Returns the number of dimensions in the array variable.

  @return  an Integer

**)
function TVBVar.GetDimensions: Integer;
begin
  Result := High(FDimensions) - Low(FDimensions) + 1;
end;

End.

(**

  This module contains a class which implements an Object Pascal method declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    10 Mar 2018

**)
Unit BADI.Pascal.MethodDecl;

Interface

Uses
  Classes,
  BADI.Generic.MethodDecl,
  BADI.Pascal.RecordDecl,
  BADI.ElementContainer,
  BADI.Types,
  BADI.TokenInfo;

Type
  (** This is a class that defines method within Object Pascal code. **)
  TPascalMethod = Class(TGenericMethodDecl)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDirectives           : TStringList;
    FResolved             : Boolean;
    FRecObjClsInt         : TRecordDecl;
    FTypesLabel           : TLabelContainer;
    FVariablesLabel       : TLabelContainer;
    FConstantsLabel       : TLabelContainer;
    FResourceStringsLabel : TLabelContainer;
    FLabelsLabel          : TLabelContainer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(Const MethodType : TMethodType; Const strName : String; Const AScope : TScope;
      Const iLine, iCol : Integer); Override;
    Destructor Destroy; Override;
    Procedure AddDirectives(Const strDirective : String);
    Function HasDirective(Const strDirective : String) : Boolean;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Function GetQualifiedName : String; Override;
    (**
      Returns the string list of directives associated with the method.
      @precon  None.
      @postcon Returns the string list of directives associated with the method.
      @return  a TStringList
    **)
    Property Directives : TStringList Read FDirectives;
    (**
      This property returns whether the method is resovled forward or not.
      @precon  None.
      @postcon Returns whether the method is resovled forward or not.
      @return  a Boolean
    **)
    Property Resolved : Boolean Read FResolved Write FResolved;
    (**
      This property returns the record/object/class that the method belongs to.
      @precon  None.
      @postcon Returns the record/object/class that the method belongs to.
      @return  a TRecordDecl
    **)
    Property RecObjClsInt : TRecordDecl Read FRecObjClsInt Write FRecObjClsInt;
    (**
      This property gets or sets the types label for the module.
      @precon  None.
      @postcon Gets or sets the types label for the module.
      @return  a TLabelContainer
    **)
    Property TypesLabel  : TLabelContainer Read FTypesLabel Write FTypesLabel;
    (**
      This property gets or sets the Variables label for the module.
      @precon  None.
      @postcon Gets or sets the Variables label for the module.
      @return  a TLabelContainer
    **)
    Property VariablesLabel : TLabelContainer Read FVariablesLabel
      Write FVariablesLabel;
    (**
      This property gets or sets the Constants label for the module.
      @precon  None.
      @postcon Gets or sets the Constants label for the module.
      @return  a TLabelContainer
    **)
    Property ConstantsLabel : TLabelContainer Read FConstantsLabel
      Write FConstantsLabel;
    (**
      This property gets or sets the Resource Strings label for the module.
      @precon  None.
      @postcon Gets or sets the Resource Strings label  for the module.
      @return  a TLabelContainer
    **)
    Property ResourceStringsLabel : TLabelContainer Read FResourceStringsLabel
      Write FResourceStringsLabel;
    (**
      This property gets or sets the Labels label for the module.
      @precon  None.
      @postcon Gets or sets the Labels label for the module.
      @return  a TLabelContainer
    **)
    Property LabelsLabel : TLabelContainer Read FLabelsLabel Write FLabelsLabel;
  End;

Implementation

Uses
  BADI.Constants,
  BADI.Pascal.Constants,
  BADI.Pascal.Functions,
  SysUtils;

(**

  This method adds a directive to the directives list.

  @precon  strDirective is a directive token to be added to the directives
           collection.
  @postcon Adds a directive to the directives list.

  @param   strDirective as a String as a Constant

**)
Procedure TPascalMethod.AddDirectives(Const strDirective : String);

Begin
  FDirectives.Add(strDirective);
End;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Outputs the pascal method declaration.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TPascalMethod.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strClass = 'Class';
  
Var
  i : Integer;

begin
  Result := strMethodTypes[MethodType];
  If ClassMethod Then
    Result := strClass + ' ' + Result;
  If Name <> '' Then
    If boolShowIdentifier And (Identifier <> '') Then
      Result := Result + #32 + Identifier;
  If ParameterCount > 0 Then
    Begin
      If boolForDocumentation Then
        Result := Result + '('#13#10
      Else
        Result := Result + '(';
      Result := Result + BuildParameterRepresentation(Self, boolShowIdentifier,
        boolForDocumentation);
      If boolForDocumentation Then
        Result := Result + #13#10;
      Result := Result + ')';
    End;
  If ReturnType.ElementCount > 0 Then
      Result := Result + #32':'#32 + ReturnType.AsString(False, boolForDocumentation);
  For i := 0 To FDirectives.Count - 1 Do
    Result := Result + '; ' + FDirectives[i];
end;

(**

  This is the constructor method for the TPascalMethod class.

  @precon  None.
  @postcon Initialises the class and creates a string list for the directives.

  @nometricMissingCONSTInParam

  @param   MethodType as a TMethodType as a constant
  @param   strName    as a String as a constant
  @param   AScope     as a TScope as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant

**)
Constructor TPascalMethod.Create(Const MethodType : TMethodType; Const strName : String;
  Const AScope : TScope; Const iLine, iCol : Integer);

Begin
  Inherited Create(MethodType, strName, AScope, iLine, iCol);
  FTypesLabel := Nil;
  FVariablesLabel := Nil;
  FConstantsLabel := Nil;
  FResourceStringsLabel := Nil;
  FLabelsLabel := Nil;
  FDirectives := TStringList.Create;
  FResolved := False;
  FRecObjClsInt := Nil;
End;

(**

  This is the destructor method for the TPascalMethod class.

  @precon  None.
  @postcon Frees the memory used for the directives string list.

**)
destructor TPascalMethod.Destroy;
begin
  FDirectives.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns a combination of the Identifier + parameters so that
           overloaded methods can be accepted by the container.

  @return  a String

**)
function TPascalMethod.GetName: String;

Const
  iRandomLimit = 9999;
  strProcFmt = 'PROC%4.4d';
  strForward = 'forward';

Var
  i : Integer;

begin
  Result := Identifier;
  If Result = '' Then
    Result := Format(strProcFmt, [Random(iRandomLimit)]);
  For i := 0 To ParameterCount - 1 Do
    Begin
      Result := Result + Format('!P%d!', [i]) + strParamModifier[Parameters[i].ParamModifier];
      If Parameters[i].ParamType <> Nil Then
        Begin
          Result := Result + strArrayOf[Parameters[i].ArrayOf];
          Result := Result + Parameters[i].ParamType.AsString(False, False);
        End;
    End;
  If ReturnType.ElementCount > 0 Then
    Result := Result + '!R!' + ReturnType.AsString(False, False);
  If HasDirective(strForward) Then
    Result := Result + '!' + strForward;
end;

(**

  This is a getter method for the QualifiedName property.

  @precon  None.
  @postcon For anonymous methods, the parent qualified name is pre-pended.

  @return  a String

**)
Function TPascalMethod.GetQualifiedName: String;

ResourceString
  strAnonymous = 'Anonymous';

Begin
  Result := Inherited GetQualifiedName;
  If Length(Result) = 0 Then
    Begin
      Result := strAnonymous;
      If Assigned(Parent) And (Parent Is TGenericMethodDecl) Then
        Result := (Parent As TGenericMethodDecl).QualifiedName + '.' + Result;
    End;
End;

(**

  This method test the directive for a specified directive and returns true if
  found.

  @precon  strDirective is the directive to search for.
  @postcon Returns true if the directive was found.

  @param   strDirective as a String as a Constant
  @return  a Boolean

**)
function TPascalMethod.HasDirective(Const strDirective: String): Boolean;

Var
  i : Integer;

begin
  Result := False;
  For i := 0 To Directives.Count - 1 Do
    If CompareText(strDirective,
      Copy(Directives[i], 1, Length(strDirective))) = 0 Then
      Begin
        Result := True;
        Break;
      End;
end;

(**

  This method tries to find the symbol with its scope as mark it as referenced.

  @precon  None.
  @postcon Tries to find the symbol with its scope as mark it as referenced.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TPascalMethod.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

Var
  i: Integer;
  M: TPascalMethod;
  boolFound: Boolean;

begin
  Result := Inherited ReferenceSymbol(AToken);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FVariablesLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FConstantsLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FResourceStringsLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FTypesLabel);
  // Local Methods
  boolFound := False;
  For i := 1 To ElementCount Do
    If Elements[i] Is TPascalMethod Then
      Begin
        M := Elements[i] As TPascalMethod;
        If CompareText(AToken.Token, M.Identifier) = 0 Then
          Begin
            M.Referenced := True;
            AToken.Reference := trResolved;
            boolFound := True;
          End;
      End;
  If boolFound Then
    Begin
      Result := True;
      Exit;
    End;
  If Assigned(RecObjClsInt) Then
    Result := RecObjClsInt.ReferenceSymbol(AToken);
  If Result Then
    Exit;
end;

End.

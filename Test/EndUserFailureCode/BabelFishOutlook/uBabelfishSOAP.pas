Unit uBabelfishSOAP;

interface

type

  BabelFishPortType = interface(IInvokable)
    ['{E2044580-7383-49EA-B205-F45337A497D7}']
    function BabelFish(const translationmode: string; const sourcedata: string): string; stdcall;
  end;


implementation

uses InvokeRegistry;

initialization
  InvRegistry.RegisterInterface(TypeInfo(BabelFishPortType));

end.
 
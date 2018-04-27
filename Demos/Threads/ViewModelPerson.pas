unit ViewModelPerson;

interface

uses
system.Classes,  System.SysUtils, System.Messaging, BusinessInterfaces;

type

  TPersonChanged = class(TMessage<IPerson>)
  end;

implementation

end.

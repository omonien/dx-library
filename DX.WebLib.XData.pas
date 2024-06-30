unit DX.WebLib.XData;

interface

uses
  System.SysUtils, System.Classes, System.Types,
  JS, Web,
  Data.DB,
  XData.Web.JsonDataset,
  XData.Web.Dataset,
  XData.Web.Client,
  DX.WebLib.Utils;

type
  /// <summary>
  /// This helper introduces a "RefreshData" method, that loads or refreshes the configures entity set from the server and provides a "ADoneProc", to implement sequential behaviour.
  /// </summary>
  TTXDataWebDataSetHelper = class helper for TXDataWebDataSet
  public
    /// <summary>
    /// RefreshData is very similiar to "Load", but offers an additional callback, once the dataset finished loading its data form the server. <br />
    /// </summary>
    [async]
    procedure RefreshData(ADoneProc: TProc); overload;

    [async]
    procedure RefreshData(const AServiceOperation: string; AParams: Array of JSValue; ADoneProc: TProc); overload;
  end;

  TXDataClientResponseHelper = class helper for TXDataClientResponse
    function ResultValue: JSValue;
  end;

  function XDataClientResponse(AValue: JSValue): TXDataClientResponse;

implementation

{ TTXDataWebDataSetHelper }

{$IFDEF PAS2JS}

// Class cracker to access protected "OnSuccess" method
type
  TXDataDataRequestX = class(TXDataDataRequest);
{$ENDIF}

procedure TTXDataWebDataSetHelper.RefreshData(ADoneProc: TProc);
// Some helper functions, taken from XData.Web.Dataset Javascript version
{$IFDEF PAS2JS}
  function QueryToObject(const Query: string): TJSObject;
  var
    Params: TStringDynArray;
    P: Integer;
    I: Integer;
  begin
    Result := TJSObject.new;
    Params := TJSString(Query).split('&');
    for I := 0 to Length(Params) - 1 do
    begin
      P := Pos('=', Params[I]);
      if P > 1 then
        Result[Copy(Params[I], 1, P - 1)] := Copy(Params[I], P + 1, MaxInt);
    end;
  end;

  function ObjectToQuery(Obj: TJSObject): string;
  var
    Key: string;
  begin
    Result := '';
    for Key in TJSObject.keys(Obj) do
    begin
      if Result <> '' then
        Result := Result + '&';
      Result := Result + Key + '=' + JS.ToString(Obj[Key]);
    end;
  end;

  function BuildQueryString: string;
  var
    Query: TJSObject;
  begin

    Query := QueryToObject(Self.QueryString);
    if Self.QueryTop <> 0 then
      Query['$top'] := IntToStr(Self.QueryTop);
    if Self.QuerySkip <> 0 then
      Query['$skip'] := IntToStr(Self.QuerySkip);
    if Self.ServerRecordCountMode = smInlineCount then
      Query['$inlinecount'] := 'allpages';
    Result := ObjectToQuery(Query);
  end;

{$ELSE}
  function BuildQueryString: string;
  begin
    // Implemented with JS only
    Result := '';
  end;
{$ENDIF}

var
  LClient: TXDataWebClient;
begin
  if Self.EntitySetName = '' then
    raise Exception.Create(Self.Name + ': Entity set not assigned!');

  console.debug('Refreshing ' + Self.EntitySetName);

  if not Assigned(Self.Connection) then
    raise Exception.Create('Connection not assigned!');
  await(Connection.OpenAsync);

  LClient := TXDataWebClient.Create(nil);
  try
    LClient.Connection := Self.Connection;
    LClient.List(EntitySetName, BuildQueryString,
      procedure(AResponse: TXDataClientResponse)
{$IFDEF PAS2JS}
      // Implemented with JS only
      var
        LDataProxy: TXDataDataProxy;
{$ENDIF}
      begin
        if (AResponse.StatusCode = 200) then
        begin

          Close;
          Self.SetJsonData(AResponse.Result);
          // FServerRecordCount is private and can only be set on JS with some trickery
{$IFDEF PAS2JS}
          LDataProxy := TXDataDataProxy.Create(Self);
          try
            TXDataDataRequestX(LDataProxy.GetDataRequest([], nil, nil)).OnSuccess(AResponse);
          finally
            FreeAndNil(LDataProxy);
          end;
{$ENDIF}
          Self.Open;
          console.debug(Self.EntitySetName + ' refreshed.');
          console.debug('RecordCount: ' + Self.RecordCount.ToString + ' ServerRecordCount: ' +
            Self.ServerRecordCount.ToString);
          if Assigned(ADoneProc) then
          begin
            ADoneProc;
          end;
        end
        else
        begin
          // Todo: OnErrorProc?
          console.error('RefreshData failed: ' + Self.Name + ' - ' + AResponse.Response.StatusReason);
        end;
      end);
  finally
    FreeAndNil(LClient);
  end;
end;

procedure TTXDataWebDataSetHelper.RefreshData(const AServiceOperation: string; AParams: Array of JSValue;
  ADoneProc: TProc);
var
  LResponse: TXDataClientResponse;
  LClient: TXDataWebClient;
begin
  if not Assigned(Connection) then
  begin
    console.error(Self.Name + ': Connection not assigned');
    exit;
  end;
  Self.Close;
  LClient := TXDataWebClient.Create(nil);
  try
    LClient.Connection := Self.Connection;
    LResponse := XDataClientResponse(await(LClient.RawInvokeAsync(AServiceOperation, AParams)));
    if LResponse.Response.StatusCode >= 300 then
    begin
      console.error(AServiceOperation + ': ' + LResponse.Response.StatusReason);
      exit;
    end;
    // ResultValue ist ein Helper der das 'Value'-Objekt liefert.
    Self.SetJsonData(LResponse.ResultValue);
    Self.Open;
    console.debug(AServiceOperation + ' RecCount: ' + Self.RecordCount.ToString);
  finally
    FreeAndNil(LClient);
  end;
  if Assigned(ADoneProc) then
  begin
    ADoneProc;
  end;

end;

function XDataClientResponse(AValue: JSValue): TXDataClientResponse;
begin
  result := TXDataClientResponse(AValue);
end;

function TXDataClientResponseHelper.ResultValue: JSValue;
begin
  result := self.ResultAsObject['value'];
end;

end.

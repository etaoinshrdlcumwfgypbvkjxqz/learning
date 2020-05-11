unit UnitGame;

{$mode objfpc}{$H+}
{$modeSwitch AdvancedRecords}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Forms,
  LCLType, OpenGLContext, Controls, StdCtrls, ComCtrls,
  fgl, Matrix, GL, GLU, MTProcs,
  UnitUtilities;

resourcestring
  ResStringEUnexpected = 'Unexpected';
  ResStringEIllegal = 'Illegal';

type
  { World: Forward }

  TEnumFacing = (FacingUp, FacingDown, FacingNorth, FacingEast, FacingSouth, FacingWest);
  TSetEnumFacing = set of TEnumFacing;
  TArrayGLLists = array of GLuint;

  { TGPosition }

  generic TGPosition<T> = class
    protected
      fx: T;
      fy: T;
      fz: T;
    public
      constructor Create(const x, y, z: T);
      property x: T read fx;
      property y: T read fy;
      property z: T read fz;
      function Clone: TGPosition; virtual;
      function Offset(Facing: TEnumFacing): TGPosition; virtual;
      function Up: TGPosition; virtual;
      function Down: TGPosition; virtual;
      function North: TGPosition; virtual;
      function East: TGPosition; virtual;
      function South: TGPosition; virtual;
      function West: TGPosition; virtual;
  end;
  TPositionInteger = specialize TGPosition<Integer>;
  PPositionInteger = ^TPositionInteger;

type
  TPositionGLdouble = specialize TGPosition<GLdouble>;
  TBlock = class;
  TWorld = class;
  TBlockAir = class;
  TBlockExist = class;
  TBlockOpaque = class;
  TBlockTranslucent = class;

  { Renderer: Forward }

  TCamera = class;
  TBlockRenderer = class;
  TBlockRendererDefault = class;
  TBlockAirRenderer = class;
  TBlockOpaqueRenderer = class;
  TBlockTranslucentRenderer = class;
  TWorldRenderer = class;

  { TFPGMapVisibleKey }

  TFPGMapVisibleKey = packed record
    Value: TPositionInteger;
    class operator = (const Left, Right: TFPGMapVisibleKey): boolean; overload;
    class operator < (const Left, Right: TFPGMapVisibleKey): boolean; overload;
    class operator > (const Left, Right: TFPGMapVisibleKey): boolean; overload;
  end;
  PFPGMapVisibleKey = ^TFPGMapVisibleKey;

  { TFPGMapVisibleValue }

  TFPGMapVisibleValue = packed class
    private
      FGLList: GLuint;
    public
      Facings: TSetEnumFacing;
      constructor Create;
      function GetCallList: GLuint;
      procedure SetCallList(const list: GLuint);
    private
      FGLListExists: boolean;
    public
      property GLList: GLuint read GetCallList write SetCallList;
      property GLListExists: boolean read FGLListExists;
  end;
  PFPGMapVisibleValue = ^TFPGMapVisibleValue;

type
  TFPGMapVisible = specialize TFPGMap<PFPGMapVisibleKey, TFPGMapVisibleValue>;
function CompareTFPGMapVisibleKey(const PLeft, PRight: PFPGMapVisibleKey): Integer;

type
  { World }

  { TBlock }

  TBlock = class
    protected
      FRenderer: TBlockRenderer;
    public
      constructor Create(const Renderer: TBlockRenderer = nil);
      procedure SetRenderer(const Renderer: TBlockRenderer);
      property Renderer: TBlockRenderer read FRenderer write SetRenderer;
      function IsAir: boolean; virtual; abstract;
      function IsOpaque: boolean; virtual; abstract;
      destructor Destroy; override;
  end;
  TArrayBlockColumn = array of TBlock;
  TArrayBlockLayer = array of TArrayBlockColumn;
  TArrayBlockCuboid = array of TArrayBlockLayer;

  { TBlockAir }

  TBlockAir = class(TBlock)
    function IsAir: boolean; override;
    function IsOpaque: boolean; override;
  end;

  { TBlockExist }

  TBlockExist = class(TBlock)
    function IsAir: boolean; override;
  end;

  { TBlockOpaque }

  TBlockOpaque = class(TBlockExist)
    function IsOpaque: boolean; override;
  end;

  { TBlockTranslucent }

  TBlockTranslucent = class(TBlockExist)
    function IsOpaque: boolean; override;
  end;

  { TWorld }

  TFunctionWorldForeachBlock = function(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock;
  TFunctionWorldForeachBlockOfObject = function(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock of object;
  TFunctionBlockForeachNeighbor = function(BlockTo, BlockFrom: TBlock; PositionTo, PositionFrom: TPositionInteger; Facing: TEnumFacing; Arg: Pointer): TBlock;
  TFunctionBlockForeachNeighborOfObject = function(BlockTo, BlockFrom: TBlock; PositionTo, PositionFrom: TPositionInteger; Facing: TEnumFacing; Arg: Pointer): TBlock of object;
  TWorld = class
    protected
      FRenderer: TWorldRenderer;
      FSize: Longint;
    public
      constructor Create(const Size: Longint; const Renderer: TWorldRenderer = nil);
      procedure SetRenderer(const Renderer: TWorldRenderer);
      property Renderer: TWorldRenderer read FRenderer write SetRenderer;
      property Size: Longint read FSize;
      function IsValidPosition(const Position: TPositionInteger): boolean; virtual;
      function GetBlock(const Position: TPositionInteger): TBlock; virtual;
      procedure SetBlock(const Position: TPositionInteger; const Block: TBlock); virtual;
      procedure ForeachBlock(const f: TFunctionWorldForeachBlock; const Concurrent: boolean = false; const Arg: Pointer = nil); virtual; overload;
      procedure ForeachBlock(const f: TFunctionWorldForeachBlockOfObject; const Concurrent: boolean = false; const Arg: Pointer = nil); virtual; overload;
      procedure ForeachNeighbor(const Position: TPositionInteger; const f: TFunctionBlockForeachNeighbor; const Arg: Pointer = nil); overload;
      procedure ForeachNeighbor(const Position: TPositionInteger; const f: TFunctionBlockForeachNeighborOfObject; const Arg: Pointer = nil); overload;
    protected
      Data: TArrayBlockCuboid;
      function InitializeData(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock; virtual;
    public
      destructor Destroy; override;
  end;

  { Renderer }

  { TCamera }

  TCamera = class
    public
      fov, zNear, zFar: GLdouble;
      constructor Create(const World: TWorld = nil);
      procedure SetWorld(const World: TWorld);
      procedure HandleRotateMouse(const Sender: TOpenGLControl; const Shift: TShiftState; const x, y: Integer);
      procedure HandleRotateKeyboard(const Sender: TOpenGLControl; const TimeDeltaMills: double);
      procedure UpdateRotation;
      procedure Move(const Sender: TOpenGLControl; const TimeDeltaMills: double);
      procedure Render(const Sender: TOpenGLControl);
      function GetPosition: Tvector3_double;
      destructor Destroy; override;
    protected
      FWorld: TWorld;
      AxisX, AxisY, AxisZ: ^Tvector3_double;
      FPosition: ^Tvector3_double;
      Rotation: ^TQuaternionDouble;
      Front, Left, Up: ^Tvector3_double;
    public
      property Position: Tvector3_double read GetPosition;
      property World: TWorld read FWorld write SetWorld;
  end;

  { TBlockRenderer }

  TBlockRenderer = class
    protected
      FBlock: TBlock;
    public
      constructor Create(const Block: TBlock = nil);
      procedure SetBlock(const Block: TBlock);
      property Block: TBlock read FBlock write SetBlock;
      procedure Render(const Position: TPositionInteger; const Facings: TSetEnumFacing); virtual; abstract;
      destructor Destroy; override;
  end;

  { TBlockRendererDefault }

  TBlockRendererDefault = class(TBlockRenderer)
    public
      procedure Render(const Position: TPositionInteger; const Facings: TSetEnumFacing); override;
      procedure RenderUp(const Position: TPositionInteger); virtual;
      procedure RenderDown(const Position: TPositionInteger); virtual;
      procedure RenderNorth(const Position: TPositionInteger); virtual;
      procedure RenderEast(const Position: TPositionInteger); virtual;
      procedure RenderSouth(const Position: TPositionInteger); virtual;
      procedure RenderWest(const Position: TPositionInteger); virtual;
  end;

  { TBlockAirRenderer }

  TBlockAirRenderer = class(TBlockRendererDefault)
    public
      procedure Render(const Position: TPositionInteger; const Facings: TSetEnumFacing); override;
  end;

  { TBlockOpaqueRenderer }

  TBlockOpaqueRenderer = class(TBlockRendererDefault)
    public
      procedure Render(const Position: TPositionInteger; const Facings: TSetEnumFacing); override;
  end;

  { TBlockTranslucentRenderer }

  TBlockTranslucentRenderer = class(TBlockRendererDefault)
    public
      procedure Render(const Position: TPositionInteger; const Facings: TSetEnumFacing); override;
  end;

  { TWorldRenderer }

  TWorldRenderer = class
    protected
      FWorld: TWorld;
      VisibleOpaque: TFPGMapVisible;
      VisibleTranslucent: TFPGMapVisible;
      GLListsOpaque: TArrayGLLists;
      GLListsTranslucent: TArrayGLLists;
      GLListsOpaqueReady: boolean;
      GLListsTranslucentReady: boolean;
      RTLCriticalSectionVisible: TRTLCriticalSection;
      function InitializeVisible(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock;
      type TCheckNeighborBlockForVisibleParameters = packed record
        Value: TFPGMapVisibleValue;
      end;
      type PCheckNeighborBlockForVisibleParameters = ^TCheckNeighborBlockForVisibleParameters;
      function CheckNeighborBlockForVisible(BlockTo, BlockFrom: TBlock; PositionTo, PositionFrom: TPositionInteger; Facing: TEnumFacing; Arg: Pointer): TBlock;
    public
      constructor Create(const World: TWorld = nil);
      procedure SetWorld(const World: TWorld);
      property World: TWorld read FWorld write SetWorld;
      procedure Render;
      destructor Destroy; override;
  end;

  { Control }

  TKeyRange = 0..$FF;
  TSetKey = set of TKeyRange;

{ UnitGame }

procedure OnIdle0(const Sender: TOpenGLControl; var Done: boolean);
procedure OnResize0(const Sender: TOpenGLControl);
procedure Draw0(const Sender: TOpenGLControl);

{ TApply0Parameters }

type
  TApply0Parameters = packed record
    Size: Longint;
    Sender: TOpenGLControl;
  end;
  PApply0Parameters = ^TApply0Parameters;

{ UnitGame }

procedure Apply1(const Sender: TOpenGLControl; const Size: Longint; const ProgressBarLoading: TProgressBar);
function Apply0(Parameter: Pointer): PtrInt;
procedure Apply0LambdaProgressBar(Data: PtrInt);
procedure KeyDown0(const Sender: TOpenGLControl; var Key: Word; const Shift: TShiftState);
procedure KeyUp0(const Sender: TOpenGLControl; var Key: Word; const Shift: TShiftState);
procedure OnClick0(const Sender: TOpenGLControl);
procedure OnMouseMove0(const Sender: TOpenGLControl; const Shift: TShiftState; const X, Y: Integer);
procedure OnExit0(const Sender: TOpenGLControl);

procedure MouseKeyboardCapture(const Sender: TOpenGLControl);
procedure MouseKeyboardUncapture(const Sender: TOpenGLControl);

const
  { World }
  { Render }
  { Control }
  SetKeyRange = [Low(TKeyRange)..High(TKeyRange)];
  KeysActive = [VK_W, VK_S, VK_A, VK_D, VK_Q, VK_E, VK_SPACE, VK_SHIFT];
  CameraMovementPerMills = 10 / MSecsPerSec;
  CameraRotationPerPixel = 90 / 800;
  CameraRotationPerMills = 90 / MSecsPerSec;

var
  TimeDeltaMills: double = -1;

implementation

var
  RTLCriticalSectionLoading: TRTLCriticalSection;
  ProgressBar: TProgressBar;

  RenderControlCenter: TPoint;
  TimeRenderedDay: TDateTime;
  KeysBeingPressed: TSetKey;
  MouseKeyboardCaptured: boolean = false;

  Model, Proj: T16dArray;
  View: TViewPortArray;

  ICamera: TCamera;
  IWorld: TWorld;
  IWorldRenderer: TWorldRenderer;
  IBlockAir: TBlock;
  IBlockAirRenderer: TBlockRenderer;
  IBlockOpaque: TBlock;
  IBlockOpaqueRenderer: TBlockRenderer;
  IBlockTranslucent: TBlock;
  IBlockTranslucentRenderer: TBlockRenderer;

{ World }

{ TGPosition }

constructor TGPosition.Create(const x, y, z: T);
begin
  fx:=x;
  fy:=y;
  fz:=z;
end;
function TGPosition.Clone: TGPosition;
begin
  exit(TGPosition.Create(x, y, z));
end;
function TGPosition.Offset(Facing: TEnumFacing): TGPosition;
begin
  case Facing of
    FacingUp: exit(Up);
    FacingDown: exit(Down);
    FacingNorth: exit(North);
    FacingEast: exit(East);
    FacingSouth: exit(South);
    FacingWest: exit(West);
    else raise EAbort(ResStringEUnexpected);
  end;
end;
function TGPosition.Up: TGPosition;
begin
  exit(TGPosition.Create(x, y, z + 1));
end;
function TGPosition.Down: TGPosition;
begin
  exit(TGPosition.Create(x, y, z - 1));
end;
function TGPosition.North: TGPosition;
begin
  exit(TGPosition.Create(x + 1, y, z));
end;
function TGPosition.East: TGPosition;
begin
  exit(TGPosition.Create(x, y - 1, z));
end;
function TGPosition.South: TGPosition;
begin
  exit(TGPosition.Create(x - 1, y, z));
end;
function TGPosition.West: TGPosition;
begin
  exit(TGPosition.Create(x, y + 1, z));
end;

{ TFPGMapVisibleKey }

class operator TFPGMapVisibleKey.= (const Left, Right: TFPGMapVisibleKey): boolean;
begin
  Result:=(Left.Value.x = Right.Value.x) and (Left.Value.y = Right.Value.y) and (Left.Value.z = Right.Value.z);
end;
class operator TFPGMapVisibleKey.< (const Left, Right: TFPGMapVisibleKey): boolean;
var
  LeftPosition, RightPosition: Tvector3_double;
begin
  with Left.Value do LeftPosition.Init(x, y, z);
  with Right.Value do RightPosition.Init(x, y, z);
  Result:=(LeftPosition - ICamera.Position).Squared_Length < (RightPosition - ICamera.Position).Squared_Length;
end;
class operator TFPGMapVisibleKey.> (const Left, Right: TFPGMapVisibleKey): boolean;
var
  LeftPosition, RightPosition: Tvector3_double;
begin
  with Left.Value do LeftPosition.Init(x, y, z);
  with Right.Value do RightPosition.Init(x, y, z);
  Result:=(LeftPosition - ICamera.Position).Squared_Length > (RightPosition - ICamera.Position).Squared_Length;
end;
function CompareTFPGMapVisibleKey(const PLeft, PRight: PFPGMapVisibleKey): Integer;
begin
  if PLeft^ = PRight^ then exit(0)
  else if PLeft^ < PRight^ then exit(1)
  else { if PLeft^ > PRight^ then } exit(-1);
end;

{ TFPGMapVisibleValue }

constructor TFPGMapVisibleValue.Create;
begin
  FGLList:=0;
  Facings:=[];
  FGLListExists:=false;
end;
function TFPGMapVisibleValue.GetCallList: GLuint;
begin
  if not GLListExists then raise EAbort.Create(ResStringEIllegal);
  exit(FGLList);
end;
procedure TFPGMapVisibleValue.SetCallList(const list: GLuint);
begin
  if GLListExists then glDeleteLists(GLList, 1);
  FGLList:=list;
  FGLListExists:=true;
end;

{ TBlock }

constructor TBlock.Create(const Renderer: TBlockRenderer = nil);
begin
  Self.Renderer:=Renderer;
end;
procedure TBlock.SetRenderer(const Renderer: TBlockRenderer);
begin
  if (Self.Renderer <> nil) and (Renderer <> nil) then raise EAbort.Create(ResStringEIllegal);
  FRenderer:=Renderer;
end;
destructor TBlock.Destroy;
begin
  try
    if Renderer <> nil then
    begin
      Renderer.Block:=nil;
      Renderer.Destroy;
    end;
  finally
    inherited;
  end;
end;

{ TBlockAir }

function TBlockAir.IsAir: boolean;
begin
  exit(true);
end;
function TBlockAir.IsOpaque: boolean;
begin
  exit(false);
end;

{ TBlockExist }

function TBlockExist.IsAir: boolean;
begin
  exit(false);
end;

function TBlockOpaque.IsOpaque: boolean;
begin
  exit(true);
end;

{ TBlockTranslucent }

function TBlockTranslucent.IsOpaque: boolean;
begin
  exit(false);
end;

{ TWorld }

constructor TWorld.Create(const Size: Longint; const Renderer: TWorldRenderer = nil);
var
  x, y: Integer;
begin
  FSize:=Size;
  SetLength(Data, Size);
  for x:=0 to Size - 1 do
  begin
    SetLength(Data[x], Size);
    for y:=0 to Size - 1 do
    begin
      SetLength(Data[x][y], Size);
    end;
  end;
  ForeachBlock(@InitializeData, true);
  Self.Renderer:=Renderer;
end;
procedure TWorld.SetRenderer(const Renderer: TWorldRenderer);
begin
  if (Self.Renderer <> nil) and (Renderer <> nil) then raise EAbort.Create(ResStringEIllegal);
  FRenderer:=Renderer;
end;
function TWorld.InitializeData(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock;
begin
  case Random(3) of
    0: exit(IBlockAir);
    1: exit(IBlockOpaque);
    2: exit(IBlockTranslucent);
    else raise EAbort.Create(ResStringEUnexpected);
  end;
end;
function TWorld.IsValidPosition(const Position: TPositionInteger): boolean;
begin
  with Position do
  begin
    exit((x >= 0) and (x < Size) and (y >=0) and (y < Size) and (z >= 0) and (z < Size));
  end;
end;
function TWorld.GetBlock(const Position: TPositionInteger): TBlock;
begin
  if IsValidPosition(Position) then
  begin
    exit(Data[Position.x][Position.y][Position.z]);
  end
  else exit(nil);
end;
procedure TWorld.SetBlock(const Position: TPositionInteger; const Block: TBlock);
begin
  if Block = nil then exit;
  if IsValidPosition(Position) then
  begin
    Data[Position.x][Position.y][Position.z]:=Block;
  end;
end;
procedure TWorld.ForeachBlock(const f: TFunctionWorldForeachBlock; const Concurrent: boolean = false; const Arg: Pointer = nil);
  procedure ForeachBlockLayer(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    y, z: Longint;
    Position: TPositionInteger;
  begin
    for y:=0 to Size - 1 do
    begin
      for z:=0 to Size - 1 do
      begin
        Position:=TPositionInteger.Create(Index, y, z);
        try
          SetBlock(Position, f(GetBlock(Position), Position, Arg));
        finally
          Position.Destroy;
        end;
      end;
    end;
  end;
var
  x: Longint;
begin
  if Concurrent then ProcThreadPool.DoParallelLocalProc(@ForeachBlockLayer, 0, Size - 1)
  else
  begin
    for x:=0 to Size - 1 do ForeachBlockLayer(x, nil, nil);
  end;
end;
procedure TWorld.ForeachBlock(const f: TFunctionWorldForeachBlockOfObject; const Concurrent: boolean = false; const Arg: Pointer = nil);
  procedure ForeachBlockLayer(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    y, z: Longint;
    Position: TPositionInteger;
  begin
    for y:=0 to Size - 1 do
    begin
      for z:=0 to Size - 1 do
      begin
        Position:=TPositionInteger.Create(Index, y, z);
        try
          SetBlock(Position, f(GetBlock(Position), Position, Arg));
        finally
          Position.Destroy;
        end;
      end;
    end;
  end;
var
  x: Longint;
begin
  if Concurrent then ProcThreadPool.DoParallelLocalProc(@ForeachBlockLayer, 0, Size - 1)
  else
  begin
    for x:=0 to Size - 1 do ForeachBlockLayer(x, nil, nil);
  end;
end;
procedure TWorld.ForeachNeighbor(const Position: TPositionInteger; const f: TFunctionBlockForeachNeighbor; const Arg: Pointer = nil);
var
  BlockFrom: TBlock;
  Facing: TEnumFacing;
  PositionTo: TPositionInteger;
begin
  BlockFrom:=GetBlock(Position);
  for Facing in TEnumFacing do
  begin
    PositionTo:=Position.Offset(Facing);
    try
      SetBlock(PositionTo, f(GetBlock(PositionTo), BlockFrom, PositionTo, Position, Facing, Arg));
    finally
      PositionTo.Destroy;
    end;
  end;
end;
procedure TWorld.ForeachNeighbor(const Position: TPositionInteger; const f: TFunctionBlockForeachNeighborOfObject; const Arg: Pointer = nil);
var
  BlockFrom: TBlock;
  Facing: TEnumFacing;
  PositionTo: TPositionInteger;
begin
  BlockFrom:=GetBlock(Position);
  for Facing in TEnumFacing do
  begin
    PositionTo:=Position.Offset(Facing);
    try
      SetBlock(PositionTo, f(GetBlock(PositionTo), BlockFrom, PositionTo, Position, Facing, Arg));
    finally
      PositionTo.Destroy;
    end;
  end;
end;
destructor TWorld.Destroy;
begin
  try
    if Renderer <> nil then
    begin
      Renderer.World:=nil;
      Renderer.Destroy;
    end;
  finally
    inherited;
  end;
end;

{ Renderer }

{ TCamera }

constructor TCamera.Create(const World: TWorld = nil);
begin
  New(AxisX, Init(1, 0, 0));
  New(AxisY, Init(0, 1, 0));
  New(AxisZ, Init(0, 0, 1));

  fov:=90;
  zNear:=0.1;
  zFar:=1000;

  New(Rotation, InitAngleAxis(0, AxisX^));
  New(Front);
  Front^:=AxisX^;
  New(Left);
  Left^:=AxisY^;
  New(Up);
  Up^:=AxisZ^;

  Self.World:=World;
end;
procedure TCamera.SetWorld(const World: TWorld);
var
  WorldCenter: double;
begin
  if (Self.World <> nil) and (World <> nil) then raise EAbort.Create(ResStringEIllegal);
  FWorld:=World;
  if Self.World <> nil then
  begin
    WorldCenter:=(0 + World.Size) / 2;
    New(FPosition, Init(WorldCenter, WorldCenter, World.Size + 1));
  end;
end;
function TCamera.GetPosition: Tvector3_double;
begin
  exit(FPosition^);
end;
procedure TCamera.HandleRotateMouse(const Sender: TOpenGLControl; const Shift: TShiftState; const x, y: Integer);
var
  QYaw, QPitch: TQuaternionDouble;
begin
  QYaw.InitAngleAxis(CameraRotationPerPixel * (x - RenderControlCenter.x), -Up^);
  QPitch.InitAngleAxis(CameraRotationPerPixel * (y - RenderControlCenter.y), Left^);
  Rotation^:=(QYaw * (QPitch * Rotation^).Normalize).Normalize;
  Mouse.CursorPos:=Sender.ControlToScreen(RenderControlCenter);
  Sender.Invalidate;
end;
procedure TCamera.HandleRotateKeyboard(const Sender: TOpenGLControl; const TimeDeltaMills: double);
var
  CameraRotation: double;
  RollDiff: double = 0;
  QRoll: TQuaternionDouble;
begin
  CameraRotation:=CameraRotationPerMills * TimeDeltaMills;
  if VK_Q in KeysBeingPressed then RollDiff:=RollDiff - CameraRotation;
  if VK_E in KeysBeingPressed then RollDiff:=RollDiff + CameraRotation;
  QRoll.InitAngleAxis(RollDiff, Front^);
  Rotation^:=(QRoll * Rotation^).Normalize;
end;
procedure TCamera.UpdateRotation;
begin
  with TMatrixUtilities do
  begin
    Front^:=Normalize(Rotation^ * AxisX^);
    Left^:=Normalize(Rotation^ * AxisY^);
    Up^:=Normalize(Rotation^ * AxisZ^);
  end;
end;
procedure TCamera.Move(const Sender: TOpenGLControl; const TimeDeltaMills: double);
var
  CameraMovement: double;
  PositionPrev: Tvector3_double;
begin
  CameraMovement:=CameraMovementPerMills * TimeDeltaMills;
  PositionPrev:=Position;
  if VK_W in KeysBeingPressed then FPosition^:=Position + Front^ * CameraMovement;
  if VK_S in KeysBeingPressed then FPosition^:=Position - Front^ * CameraMovement;
  if VK_A in KeysBeingPressed then FPosition^:=Position + Left^ * CameraMovement;
  if VK_D in KeysBeingPressed then FPosition^:=Position - Left^ * CameraMovement;
  if VK_SPACE in KeysBeingPressed then FPosition^:=Position + Up^ * CameraMovement;
  if VK_SHIFT in KeysBeingPressed then FPosition^:=Position - Up^ * CameraMovement;
  if not (Position = PositionPrev) then
  begin
    with World.Renderer do
    begin
      VisibleTranslucent.Sorted:=false;
      GLListsTranslucentReady:=false;
    end;
  end;
end;
procedure TCamera.Render(const Sender: TOpenGLControl);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  with Sender do gluPerspective(fov, Width / Height, zNear, zFar);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  with Position do
    gluLookAt(Data[v3x], Data[v3y], Data[v3z],
              Data[v3x] + Front^.Data[v3x], Data[v3y] + Front^.Data[v3y], Data[v3z] + Front^.Data[v3z],
              Up^.Data[v3x], Up^.Data[v3y], Up^.Data[v3z]);

  glGetDoublev(GL_MODELVIEW_MATRIX, @Model);
  glGetDoublev(GL_PROJECTION_MATRIX, @Proj);
  glGetIntegerv(GL_VIEWPORT, @View);
end;
destructor TCamera.Destroy;
begin
  try
    Dispose(AxisX);
    Dispose(AxisY);
    Dispose(AxisZ);

    Dispose(FPosition);
    Dispose(Rotation, CleanUp);
    Dispose(Front);
    Dispose(Left);
    Dispose(Up);
  finally
    inherited;
  end;
end;

{ TBlockRenderer }

constructor TBlockRenderer.Create(const Block: TBlock);
begin
  Self.Block:=Block;
end;
procedure TBlockRenderer.SetBlock(const Block: TBlock);
begin
  if (Self.Block <> nil) and (Block <> nil) then raise EAbort.Create(ResStringEIllegal);
  FBlock:=Block;
end;
destructor TBlockRenderer.Destroy;
begin
  try
    if Block <> nil then
    begin
      Block.Renderer:=nil;
      Block.Destroy;
    end;
  finally
    inherited;
  end;
end;

{ TBlockRendererDefault }

procedure TBlockRendererDefault.Render(const Position: TPositionInteger; const Facings: TSetEnumFacing);
begin
  if FacingUp in Facings then RenderUp(Position);
  if FacingDown in Facings then RenderDown(Position);
  if FacingNorth in Facings then RenderNorth(Position);
  if FacingEast in Facings then RenderEast(Position);
  if FacingSouth in Facings then RenderSouth(Position);
  if FacingWest in Facings then RenderWest(Position);
end;
procedure TBlockRendererDefault.RenderUp(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(    x,     y, z + 1);
    glVertex3i(x + 1,     y, z + 1);
    glVertex3i(x + 1, y + 1, z + 1);
    glVertex3i(    x, y + 1, z + 1);
  end;
end;
procedure TBlockRendererDefault.RenderDown(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(    x,     y, z);
    glVertex3i(    x, y + 1, z);
    glVertex3i(x + 1, y + 1, z);
    glVertex3i(x + 1,     y, z);
  end;
end;
procedure TBlockRendererDefault.RenderNorth(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(x + 1,     y,     z);
    glVertex3i(x + 1, y + 1,     z);
    glVertex3i(x + 1, y + 1, z + 1);
    glVertex3i(x + 1,     y, z + 1);
  end;
end;
procedure TBlockRendererDefault.RenderEast(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(    x, y,     z);
    glVertex3i(x + 1, y,     z);
    glVertex3i(x + 1, y, z + 1);
    glVertex3i(    x, y, z + 1);
  end;
end;
procedure TBlockRendererDefault.RenderSouth(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(x,     y,     z);
    glVertex3i(x,     y, z + 1);
    glVertex3i(x, y + 1, z + 1);
    glVertex3i(x, y + 1,     z);
  end;
end;
procedure TBlockRendererDefault.RenderWest(const Position: TPositionInteger);
begin
  with Position do
  begin
    glVertex3i(    x, y + 1,     z);
    glVertex3i(    x, y + 1, z + 1);
    glVertex3i(x + 1, y + 1, z + 1);
    glVertex3i(x + 1, y + 1,     z);
  end;
end;

{ TBlockAirRenderer }

procedure TBlockAirRenderer.Render(const Position: TPositionInteger; const Facings: TSetEnumFacing);
begin
  // NOOP
end;

{ TBlockOpaqueRenderer }

procedure TBlockOpaqueRenderer.Render(const Position: TPositionInteger; const Facings: TSetEnumFacing);
begin
  glColor3d(Random, Random, Random);
  inherited;
end;

{ TBlockTranslucentRenderer }

procedure TBlockTranslucentRenderer.Render(const Position: TPositionInteger; const Facings: TSetEnumFacing);
begin
  glColor4d(Random, Random, Random, 0.5);
  inherited;
end;

{ TWorldRenderer }

constructor TWorldRenderer.Create(const World: TWorld = nil);
begin
  VisibleOpaque:=TFPGMapVisible.Create;
  VisibleOpaque.OnKeyCompare:=@CompareTFPGMapVisibleKey;
  VisibleTranslucent:=TFPGMapVisible.Create;
  VisibleTranslucent.OnKeyCompare:=@CompareTFPGMapVisibleKey;
  InitCriticalSection(RTLCriticalSectionVisible);
  Self.World:=World;
end;
procedure TWorldRenderer.SetWorld(const World: TWorld);
begin
  if (Self.World <> nil) and (World <> nil) then raise EAbort.Create(ResStringEIllegal);
  FWorld:=World;
  if Self.World <> nil then Self.World.ForeachBlock(@InitializeVisible, true);
end;
function TWorldRenderer.InitializeVisible(Block: TBlock; Position: TPositionInteger; Arg: Pointer): TBlock;
var
  Parameters: TCheckNeighborBlockForVisibleParameters;
  Visible: TFPGMapVisible;
  PKey: PFPGMapVisibleKey;
begin
  Parameters.Value:=TFPGMapVisibleValue.Create;
  World.ForeachNeighbor(Position, @CheckNeighborBlockForVisible, @Parameters);

  if not (Longint(Parameters.Value.Facings) = 0) then
  begin
    if Block.IsOpaque then Visible:=VisibleOpaque
    else Visible:=VisibleTranslucent;
    New(PKey);
    PKey^.Value:=Position.Clone;
    EnterCriticalSection(RTLCriticalSectionVisible);
    Visible.Add(PKey, Parameters.Value);
    LeaveCriticalSection(RTLCriticalSectionVisible);
  end
  else Parameters.Value.Destroy;

  exit(Block);
end;
function TWorldRenderer.CheckNeighborBlockForVisible(BlockTo, BlockFrom: TBlock; PositionTo, PositionFrom: TPositionInteger; Facing: TEnumFacing; Arg: Pointer): TBlock;
var
  PParameters: PCheckNeighborBlockForVisibleParameters;
begin
  if (BlockTo = nil) or (not BlockTo.IsOpaque) then
  begin
    PParameters:=PCheckNeighborBlockForVisibleParameters(Arg);
    Include(PParameters^.Value.Facings, Facing);
  end;
  exit(BlockTo);
end;
procedure TWorldRenderer.Render;
var
  i: Integer;
  Key: TFPGMapVisibleKey;
  Value: TFPGMapVisibleValue;
begin
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if GLListsOpaqueReady then
  begin
    if not (Length(GLListsOpaque) = 0) then glCallLists(Length(GLListsOpaque), GL_UNSIGNED_INT, @GLListsOpaque[0]);
  end
  else
  begin
    with VisibleOpaque do
    begin
      SetLength(GLListsOpaque, Count);
      for i:=0 to Count - 1 do
      begin
        Key:=PFPGMapVisibleKey(Keys[i])^;
        Value:=Data[i];
        if Value.GLListExists then
        begin
          GLListsOpaque[i]:=Value.GLList;
          glCallList(Value.GLList);
        end
        else
        begin
          Value.GLList:=glGenLists(1);
          GLListsOpaque[i]:=Value.GLList;
          glNewList(Value.GLList, GL_COMPILE_AND_EXECUTE);
          glBegin(GL_QUADS);
            World.GetBlock(Key.Value).Renderer.Render(Key.Value, Value.Facings);
          glEnd;
          glEndList;
        end;
      end;
      GLListsOpaqueReady:=true;
    end;
  end;
  if GLListsTranslucentReady then
  begin
    if not (Length(GLListsTranslucent) = 0) then glCallLists(Length(GLListsTranslucent), GL_UNSIGNED_INT, @GLListsTranslucent[0]);
  end
  else
  begin
    VisibleTranslucent.Sorted:=true;
    with VisibleTranslucent do
    begin
      SetLength(GLListsTranslucent, Count);
      for i:=0 to Count - 1 do
      begin
        Key:=PFPGMapVisibleKey(Keys[i])^;
        Value:=Data[i];
        if Value.GLListExists then
        begin
          GLListsTranslucent[i]:=Value.GLList;
          glCallList(Value.GLList);
        end
        else
        begin
          Value.GLList:=glGenLists(1);
          GLListsTranslucent[i]:=Value.GLList;
          glNewList(Value.GLList, GL_COMPILE_AND_EXECUTE);
          glBegin(GL_QUADS);
            World.GetBlock(Key.Value).Renderer.Render(Key.Value, Value.Facings);
          glEnd;
          glEndList;
        end;
      end;
      GLListsTranslucentReady:=true;
    end;
  end;

  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
end;
destructor TWorldRenderer.Destroy;
var
  i: Integer;
  List: GLuint;
  PKey_: PFPGMapVisibleKey;
begin
  try
    with VisibleOpaque do
    begin
      for i:=0 to Count - 1 do
      begin
        PKey_:=PFPGMapVisibleKey(Keys[i]);
        PKey_^.Value.Destroy;
        Dispose(PKey_);
        Data[i].Destroy;
      end;
    end;
    VisibleOpaque.Destroy;
    with VisibleTranslucent do
    begin
      for i:=0 to Count - 1 do
      begin
        PKey_:=PFPGMapVisibleKey(Keys[i]);
        PKey_^.Value.Destroy;
        Dispose(PKey_);
        Data[i].Destroy;
      end;
    end;
    VisibleTranslucent.Destroy;
    for List in GLListsOpaque do glDeleteLists(List, 1);
    for List in GLListsTranslucent do glDeleteLists(List, 1);
    DoneCriticalSection(RTLCriticalSectionVisible);
    if World <> nil then
    begin
      World.Renderer:=nil;
      World.Destroy;
    end;
  finally
    inherited;
  end;
end;

{ UnitGame }

{ Render }

procedure OnIdle0(const Sender: TOpenGLControl; var Done: boolean);
var
  Key: word;
begin
  Done:=true;
  for Key in KeysActive do
  begin
    if Key in KeysBeingPressed then
    begin
      Sender.Invalidate;
      break;
    end;
  end;
end;
procedure OnResize0(const Sender: TOpenGLControl);
begin
  with RenderControlCenter do
  begin
    with Sender do
    begin
      x:=Width div 2;
      y:=Height div 2;
    end;
  end;
end;
procedure Draw0(const Sender: TOpenGLControl);
begin
  if not (TryEnterCriticalsection(RTLCriticalSectionLoading) = 0) then
  begin
    TimeDeltaMills:=MSecsPerDay * (Now - TimeRenderedDay);
    TimeRenderedDay:=Now;
    glClearColor(0.52, 0.80, 0.92, 1.0); // Sky
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    with ICamera do
    begin
      HandleRotateKeyboard(Sender, TimeDeltaMills);
      UpdateRotation;
      Move(Sender, TimeDeltaMills);
      Render(Sender);
    end;
    IWorldRenderer.Render;
    Sender.SwapBuffers;
    ProgressBar.Visible:=false;
    LeaveCriticalSection(RTLCriticalSectionLoading);
  end;
end;

{ Control }

procedure Apply1(const Sender: TOpenGLControl; const Size: Longint; const ProgressBarLoading: TProgressBar);
var
  PParameters: PApply0Parameters;
begin
  ProgressBar:=ProgressBarLoading;
  ProgressBar.Position:=0;
  ProgressBar.Visible:=true;

  New(PParameters);
  PParameters^.Sender:=Sender;
  PParameters^.Size:=Size;
  BeginThread(@Apply0, PParameters);
end;
function Apply0(Parameter: Pointer): PtrInt;
var
  PParameters: PApply0Parameters;
  LambdaProgressBar: TProcedurePointer;
begin
  PParameters:=PApply0Parameters(Parameter);
  LambdaProgressBar:=TProcedurePointer.Create(@Apply0LambdaProgressBar);

  EnterCriticalSection(RTLCriticalSectionLoading);
  @ICamera.Free;
  TThread.Queue(nil, @IWorld.Free); // Avoid GL memory leak
  Application.QueueAsyncCall(@LambdaProgressBar.Run, 10);

  ICamera:=TCamera.Create;
  IWorldRenderer:=TWorldRenderer.Create;
  Application.QueueAsyncCall(@LambdaProgressBar.Run, 30);

  IWorld:=TWorld.Create(PParameters^.Size, IWorldRenderer);
  Application.QueueAsyncCall(@LambdaProgressBar.Run, 50);

  ICamera.World:=IWorld;
  IWorldRenderer.World:=IWorld;
  Application.QueueAsyncCall(@LambdaProgressBar.Run, 90);

  TimeRenderedDay:=Now;
  LeaveCriticalSection(RTLCriticalSectionLoading);

  Application.QueueAsyncCall(@LambdaProgressBar.Run, 100);
  Application.QueueAsyncCall(@LambdaProgressBar.Destroy, 0);
  TThread.Queue(nil, @PParameters^.Sender.Invalidate);
  Dispose(PParameters);
  EndThread;
  exit(0);
end;
procedure Apply0LambdaProgressBar(Data: PtrInt);
begin
  ProgressBar.Position:=Data;
end;
procedure KeyDown0(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: MouseKeyboardUncapture(Sender);
  end;
  if not (Key in SetKeyRange) then exit;
  Sender.Refresh;
  Include(KeysBeingPressed, Key);
end;
procedure KeyUp0(const Sender: TOpenGLControl; var Key: word; const Shift: TShiftState);
begin
  if not (Key in SetKeyRange) then exit;
  Exclude(KeysBeingPressed, Key);
end;
procedure OnMouseMove0(const Sender: TOpenGLControl; const Shift: TShiftState; const x, y: Integer);
begin
  if MouseKeyboardCaptured then ICamera.HandleRotateMouse(Sender, Shift, x, y);
end;
procedure OnClick0(const Sender: TOpenGLControl);
begin
  if not MouseKeyboardCaptured then MouseKeyboardCapture(Sender);
end;
procedure OnExit0(const Sender: TOpenGLControl);
begin
  if MouseKeyboardCaptured then MouseKeyboardUncapture(Sender);
end;

procedure MouseKeyboardCapture(const Sender: TOpenGLControl);
begin
  SetCaptureControl(Sender);
  MouseKeyboardCaptured:=true;
  Mouse.CursorPos:=Sender.ControlToScreen(RenderControlCenter);
  {$IFNDEF Debug}
  Sender.Cursor:=crNone;
  {$ENDIF}
end;
procedure MouseKeyboardUncapture(const Sender: TOpenGLControl);
var
  Key: word;
begin
  SetCaptureControl(nil);
  MouseKeyboardCaptured:=false;
  Sender.Cursor:=crDefault;
  for Key in KeysBeingPressed do Exclude(KeysBeingPressed, Key);
end;

initialization
  InitCriticalSection(RTLCriticalSectionLoading);

  IBlockAirRenderer:=TBlockAirRenderer.Create;
  IBlockAir:=TBlockAir.Create(IBlockAirRenderer);
  IBlockAirRenderer.Block:=IBlockAir;
  IBlockOpaqueRenderer:=TBlockOpaqueRenderer.Create;
  IBlockOpaque:=TBlockOpaque.Create(IBlockOpaqueRenderer);
  IBlockOpaqueRenderer.Block:=IBlockOpaque;
  IBlockTranslucentRenderer:=TBlockTranslucentRenderer.Create;
  IBlockTranslucent:=TBlockTranslucent.Create(IBlockTranslucentRenderer);
  IBlockTranslucentRenderer.Block:=IBlockTranslucent;

finalization
  ICamera.Destroy;
  IWorld.Destroy;
  IBlockAir.Destroy;
  IBlockOpaque.Destroy;
  IBlockTranslucent.Destroy;

  DoneCriticalSection(RTLCriticalSectionLoading);

end.


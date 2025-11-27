unit Compression;

interface

uses
  System.SysUtils, System.Classes, System.ZLib;

function CompressData(const Data: TBytes): TBytes;
function DecompressData(const Data: TBytes): TBytes;

implementation

function CompressData(const Data: TBytes): TBytes;
var
  InputStream: TMemoryStream;
  OutputStream: TMemoryStream;
  Compressor: TZCompressionStream;
begin
  if Length(Data) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.Write(Data[0], Length(Data));
    InputStream.Position := 0;

    Compressor := TZCompressionStream.Create(OutputStream);
    try
      Compressor.CopyFrom(InputStream, InputStream.Size);
    finally
      Compressor.Free;
    end;

    SetLength(Result, OutputStream.Size);
    OutputStream.Position := 0;
    OutputStream.Read(Result[0], OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

function DecompressData(const Data: TBytes): TBytes;
var
  InputStream: TMemoryStream;
  OutputStream: TMemoryStream;
  Decompressor: TZDecompressionStream;
begin
  if Length(Data) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.Write(Data[0], Length(Data));
    InputStream.Position := 0;

    Decompressor := TZDecompressionStream.Create(InputStream);
    try
      OutputStream.CopyFrom(Decompressor, 0);
    finally
      Decompressor.Free;
    end;

    SetLength(Result, OutputStream.Size);
    OutputStream.Position := 0;
    OutputStream.Read(Result[0], OutputStream.Size);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

end.

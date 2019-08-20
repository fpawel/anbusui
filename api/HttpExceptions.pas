unit HttpExceptions;

interface

uses System.sysutils;

type
    ERpcException = class(Exception);

    ERpcNoResponseException = class(ERpcException);
    ERpcWrongResponseException = class(ERpcException);

    ERpcHTTPException = class(ERpcWrongResponseException);
    ERpcRemoteErrorException = class(ERpcException);

implementation

end.

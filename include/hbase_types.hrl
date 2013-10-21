-ifndef(_hbase_types_included).
-define(_hbase_types_included, yeah).

%% struct tCell

-record(tCell, {value :: string() | binary(),
                timestamp :: integer()}).

%% struct columnDescriptor

-record(columnDescriptor, {name :: string() | binary(),
                           maxVersions = 3 :: integer(),
                           compression = "NONE" :: string() | binary(),
                           inMemory = false :: boolean(),
                           bloomFilterType = "NONE" :: string() | binary(),
                           bloomFilterVectorSize = 0 :: integer(),
                           bloomFilterNbHashes = 0 :: integer(),
                           blockCacheEnabled = false :: boolean(),
                           timeToLive = -1 :: integer()}).

%% struct tRegionInfo

-record(tRegionInfo, {startKey :: string() | binary(),
                      endKey :: string() | binary(),
                      id :: integer(),
                      name :: string() | binary(),
                      version :: integer(),
                      serverName :: string() | binary(),
                      port :: integer()}).

%% struct mutation

-record(mutation, {isDelete = false :: boolean(),
                   column :: string() | binary(),
                   value :: string() | binary(),
                   writeToWAL = true :: boolean()}).

%% struct batchMutation

-record(batchMutation, {row :: string() | binary(),
                        mutations :: list()}).

%% struct tIncrement

-record(tIncrement, {table :: string() | binary(),
                     row :: string() | binary(),
                     column :: string() | binary(),
                     ammount :: integer()}).

%% struct tColumn

-record(tColumn, {columnName :: string() | binary(),
                  cell :: #tCell{}}).

%% struct tRowResult

-record(tRowResult, {row :: string() | binary(),
                     columns :: dict(),
                     sortedColumns :: list()}).

%% struct tScan

-record(tScan, {startRow :: string() | binary(),
                stopRow :: string() | binary(),
                timestamp :: integer(),
                columns :: list(),
                caching :: integer(),
                filterString :: string() | binary(),
                sortColumns :: boolean()}).

%% struct iOError

-record(iOError, {message :: string() | binary()}).

%% struct illegalArgument

-record(illegalArgument, {message :: string() | binary()}).

%% struct alreadyExists

-record(alreadyExists, {message :: string() | binary()}).

-endif.

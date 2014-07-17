
/**
 * The first thing to know about are types. The available types in Thrift are:
 *
 *  bool        Boolean, one byte
 *  byte        Signed byte
 *  i16         Signed 16-bit integer
 *  i32         Signed 32-bit integer
 *  i64         Signed 64-bit integer
 *  double      64-bit floating point value
 *  string      String
 *  binary      Blob (byte array)
 *  map<t1,t2>  Map from one type to another
 *  list<t1>    Ordered list of one type
 *  set<t1>     Set of unique elements of one type
 *
 * Did you also notice that Thrift supports C style comments?
 */


/**
 * Thrift files can namespace, package, or prefix their output in various
 * target languages.
 */
namespace cpp HubaThrift
namespace d HubaThrift
namespace java HubaThrift
namespace php HubaThrift
namespace perl HubaThrift
namespace hs HubaThrift

/**
 * Thrift also lets you define constants for use across languages. Complex
 * types and structs are specified using JSON notation.
 */
/**
 * You can define enums, which are just 32 bit integers. Values are optional
 * and start at 1 if not supplied, C style again.
 */

/**
 * Structs are the basic complex data structures. They are comprised of fields
 * which each have an integer identifier, a type, a symbolic name, and an
 * optional default value.
 *
 * Fields can be declared "optional", which ensures they will not be included
 * in the serialized output if they aren't set.  Note that this requires some
 * manual management in some languages.
 */

/**
 * Thrift lets you do typedefs to get pretty names for your types. Standard
 * C style here.
 */
typedef string ColumnName
typedef i32 ServerID

/** Logging data structures **/

union ColumnValue {
  1: string stringValue,
  2: i64 intValue,
  3: set<string> stringSet,
  4: list<string> stringVector,
}

struct LogMessage {
  1: required i64 timestamp,
  2: required string table,
  3: required map<ColumnName, ColumnValue> columns,
}

struct LogResponse {
  1: required i32 code,
  2: optional string message,
}

exception InvalidLogMessageException {
  1: required i32 code,
  2: required string message,
}


/** Querying data structures **/

enum AggregationFunction {
  CONSTANT = 0,

  COUNT = 1,
  MIN = 2,
  MAX = 3,
  SUM = 4,
  AVERAGE = 5,

  SUM_PER_MINUTE = 6,
  HISTOGRAM = 7,

  // IDEA: can we just pass functions in directly somehow?
  // A general aggregation function looks like State -> Column -> State
}

struct ColumnExpression {
  1: required ColumnName column,
  2: required AggregationFunction aggregationFunction = Aggregationfunction.CONSTANT,
}

enum ComparisonFunction {
  EQ = 1,
  NEQ = 2,
  REGEXP_EQ = 3,

  GT = 4,
  LT = 5,
  GTE = 6,
  LTE = 7,
}

struct Condition {
  1: required ColumnName column,
  2: required ComparisonFunction comparisonFunction,
  3: required ColumnValue value,
}

struct Query {
  1: required list<ColumnExpression> columnExpressions,
  2: required string table,
  3: required i64 timeStart
  4: required i64 timeEnd
  5: optional list <Condition> conditions = [],
  6: optional list<ColumnName> groupBy = [],
  7: optional i32 orderBy,
  8: optional i32 limit = 1000
}

union ResponseValue {
  1: string stringValue,
  2: i64 intValue,
  3: set<string> stringSet,
  4: list<string> stringVector,
  5: double doubleValue,
  6: bool isNull,
}

typedef list<ResponseValue> Row

struct QueryResponse {
  1: required i32 code,
  2: optional string message,
  3: optional list<Row> rows,
}

struct PingResponse {
  1: required i32 code,
  2: optional string message
}


/**
 * Services just need a name and can optionally inherit from another service using the extends keyword.
 */

service CommonService {
  PingResponse ping()
}

service IngestorService extends CommonService {
  LogResponse log(1:list<LogMessage> logBatch),
}

service AggregatorService extends CommonService {
  QueryResponse query(1:Query query),
}

service InternalAggregatorService extends CommonService {
  QueryResponse queryInternal(1:Query query, 2:list<ServerID> serverIDs),
}

service LeafNodeService extends CommonService {
  LogResponse log(1:required list<LogMessage> logBatch),
  QueryResponse query(1:Query query)
}

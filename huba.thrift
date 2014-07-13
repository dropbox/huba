
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

union ColumnValue {
    1: string stringValue,
    2: i64 intValue,
    3: set<string> stringSet,
    4: list<string> stringVector,
}

struct LogMessage {
  1: i64 timestamp,
  2: string table,
  3: map<ColumnName, ColumnValue> columns,
}

struct LogResponse {
    1: i32 code,
    2: string message,
}

exception InvalidLogMessageException {
  1: i32 code,
  2: string message,
}

/**
 * Services just need a name and can optionally inherit from another service using the extends keyword.
 */

service IngestorService {

  LogResponse log(1:list<LogMessage> logBatch),


}

service AggregatorService {

  /* QueryResponse query(1:Query query) */

}

service LeafNodeService {

  LogResponse log(1:required list<LogMessage> logBatch),

  /* QueryResponse query(1:Query query) */

}

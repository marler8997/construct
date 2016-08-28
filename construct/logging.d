module construct.logging;

bool verbose;
private enum LogLevel {
  debug_,
  info,
  dev,
  warning,
  error,
}
private immutable string[] logLevelPrefix =
  [
   LogLevel.debug_  : "[DEBUG] ",
   LogLevel.info    : "[INFO] ",
   LogLevel.dev     : "[DEV] ",
   LogLevel.warning : "[WARNING] ",
   LogLevel.error   : "[ERROR] ",
   ];
private void log(A...)(LogLevel level, in char[] fmt, A args)
{
  if(!__ctfe) {
    import std.stdio : writefln, stdout;
    if(verbose || (level > LogLevel.info)) {
      writefln("[PROCESSOR] " ~ logLevelPrefix[level] ~ fmt, args);
      stdout.flush();
    }
  }
}
private void pureLog(A...)(LogLevel level, in char[] fmt, A args) pure
{
  if(!__ctfe) {
    debug {
      import std.stdio : writefln, stdout;
      if(verbose || (level > LogLevel.info)) {
        writefln("[PROCESSOR] " ~ logLevelPrefix[level] ~ fmt, args);
        stdout.flush();
      }
    } else {
      throw new Exception("cannot log because program was not compiled in -debug mode");
    }
  }
}
void logDebug(A...)(in char[] fmt, A args)
{
  log(LogLevel.debug_, fmt, args);
}
void logInfo(A...)(in char[] fmt, A args)
{
  log(LogLevel.info, fmt, args);
}
void logDev(A...)(in char[] fmt, A args)
{
  log(LogLevel.dev, fmt, args);
}
void logWarning(A...)(in char[] fmt, A args)
{
  log(LogLevel.warning, fmt, args);
}
void logError(A...)(in char[] fmt, A args)
{
  log(LogLevel.error, fmt, args);
}

void pureLogDebug(A...)(in char[] fmt, A args) pure
{
  pureLog(LogLevel.debug_, fmt, args);
}
void pureLogInfo(A...)(in char[] fmt, A args) pure
{
  pureLog(LogLevel.info, fmt, args);
}
void pureLogDev(A...)(in char[] fmt, A args) pure
{
  pureLog(LogLevel.dev, fmt, args);
}
void pureLogWarning(A...)(in char[] fmt, A args) pure
{
  pureLog(LogLevel.warning, fmt, args);
}
void pureLogError(A...)(in char[] fmt, A args) pure
{
  pureLog(LogLevel.error, fmt, args);
}



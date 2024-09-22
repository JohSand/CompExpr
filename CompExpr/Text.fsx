#if INTERACTIVE
#I @"C:\Projects\Experiments\CompExpr\CompExpr\bin\Debug\net6.0"
#r "CompExpr.dll"
#endif

open CompExpr.Program

scenario {
    start "" into x
    noop 1
    next (x)
}

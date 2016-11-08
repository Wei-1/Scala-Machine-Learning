import sbtassembly.Plugin._
import AssemblyKeys._
assemblySettings

// your assembly settings here

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
    {
        case m if m.toLowerCase.matches("meta-inf") => MergeStrategy.discard
        case x => old(x)
    }
}


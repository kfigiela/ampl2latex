mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "plugin.xml"            => MergeStrategy.discard
    case "plugin.properties"     => MergeStrategy.concat
    case "META-INF/ECLIPSE_.RSA" | "META-INF/ECLIPSEF.RSA" | "META-INF/eclipse.inf" => MergeStrategy.discard
    case ".api_description"      => MergeStrategy.discard
    case ".options"              => MergeStrategy.discard
    case ".DS_Store"             => MergeStrategy.discard
    case x                       => old(x)
  }
}
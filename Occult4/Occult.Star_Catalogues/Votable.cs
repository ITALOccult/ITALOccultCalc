using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Xml;

namespace Occult.Star_Catalogues
{
	public static class Votable
	{
		public static List<StarEntry> ParseStarEntries(string returnedXmlString)
		{
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			XmlDocument xmlDocument = new XmlDocument();
			xmlDocument.LoadXml(returnedXmlString);
			if (!"VOTABLE".Equals(xmlDocument.DocumentElement!.Name))
			{
				throw new NotSupportedException("This xml file is not a valid VOTABLE document.");
			}
			string text = ((xmlDocument.DocumentElement!.Attributes["version"] != null) ? xmlDocument.DocumentElement!.Attributes["version"]!.Value : "N/A");
			if (!("1.1".Equals(text) | "1.2".Equals(text) | "1.3".Equals(text) | "1.4".Equals(text)))
			{
				MessageBox.Show("The data return from Vizier has used VOTable v" + text + ", which is not properly supported at the moment. Use the results with caution.", "VOTable Problem", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			XmlNamespaceManager xmlNamespaceManager = new XmlNamespaceManager(xmlDocument.NameTable);
			if (xmlDocument.DocumentElement!.Attributes["xmlns"] != null)
			{
				xmlNamespaceManager.AddNamespace("vt", xmlDocument.DocumentElement!.Attributes["xmlns"]!.Value);
			}
			VisieRCatalogParser visieRCatalogParser = new VisieRCatalogParser();
			List<StarEntry> list = new List<StarEntry>();
			foreach (XmlNode item in xmlDocument.SelectNodes("/vt:VOTABLE/vt:RESOURCE", xmlNamespaceManager)!)
			{
				visieRCatalogParser.ParseResourceNode(item as XmlElement, xmlNamespaceManager);
				list.AddRange(visieRCatalogParser.AllStars);
			}
			return list;
		}
	}
}

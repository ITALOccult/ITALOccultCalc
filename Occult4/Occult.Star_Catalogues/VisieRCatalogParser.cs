#define TRACE
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Text;
using System.Xml;

namespace Occult.Star_Catalogues
{
	internal class VisieRCatalogParser
	{
		private int m_idIdx = -1;

		private int[] m_idIdxMulti;

		private int m_epochRAIdx = -1;

		private int m_epochDEIdx = -1;

		private int m_epochErrorRAIdx = -1;

		private int m_epochErrorDEIdx = -1;

		private int m_raIdxCat = -1;

		private string m_raUnitCat;

		private string m_raDescCat;

		private int m_raIdxJ2000 = -1;

		private string m_raUnitJ2000;

		private string m_raDescJ2000;

		private int m_deIdxCat = -1;

		private string m_deUnitCat;

		private string m_deDescCat;

		private int m_deIdxJ2000 = -1;

		private string m_deUnitJ2000;

		private string m_deDescJ2000;

		private int m_raErrIdx = -1;

		private string m_raErrUnit;

		private string m_raErrDesc;

		private int m_deErrIdx = -1;

		private int m_radecErrPAIdx = -1;

		private string m_deErrUnit;

		private string m_deErrDesc;

		private int m_pmraIdx = -1;

		private string m_pmraUnit;

		private string m_pmraDesc;

		private int m_pmdeIdx = -1;

		private string m_pmdeUnit;

		private string m_pmdeDesc;

		private int m_pmraErrIdx = -1;

		private string m_pmraErrUnit;

		private string m_pmraErrDesc;

		private int m_pmdeErrIdx = -1;

		private string m_pmdeErrUnit;

		private string m_pmdeErrDesc;

		private int m_ruweIdx = -1;

		private string m_ruweUnit;

		private string m_ruweDesc;

		private int m_sourceIdx = -1;

		private string m_sourceUnit;

		private string m_sourceDesc;

		private int m_doubleIdx = -1;

		private string m_doubleUnit;

		private string m_doubleDesc;

		private int m_MbIdx = -1;

		private string m_MbUnit;

		private string refName_Mb;

		private int m_MvIdx = -1;

		private string m_MvUnit;

		private string refName_Mv;

		private int m_MrIdx = -1;

		private string m_MrUnit;

		private string refName_Mr;

		private int m_epochIdx = -1;

		private string m_EpochUnit;

		private string refName_Epoch;

		private string m_EpochErrorUnit;

		private string refName_EpochError;

		private string m_RVunit;

		private int m_RVIdx = -1;

		private string refName_RV;

		private int m_FlagIdx = -1;

		private string m_FlagUnit;

		private string refName_Flag;

		private int m_ParallaxIdx = -1;

		private int m_e_ParallaxIdx = -1;

		private string m_catDesgn;

		private string m_catDescr;

		private string m_tableDescr;

		private string m_equinox;

		private string m_referenceSystem;

		private string m_epochRA;

		private string m_epochDE;

		private string m_EpochUnitRA;

		private string m_EpochDescRA;

		private string m_EpochUnitDE;

		private string m_EpochDescDE;

		private string m_catID;

		private bool m_catTested;

		private List<StarEntry> m_AllStars = new List<StarEntry>();

		private XmlNamespaceManager m_nsMngr;

		public List<StarEntry> AllStars => m_AllStars;

		internal VisieRCatalogParser()
		{
		}

		public void ParseResourceNode(XmlElement resourceNode, XmlNamespaceManager nsMngr)
		{
			m_nsMngr = nsMngr;
			m_catID = ((resourceNode.Attributes["name"] == null) ? null : resourceNode.Attributes["name"]!.Value);
			XmlNode xmlNode = resourceNode.SelectSingleNode("./vt:DESCRIPTION", nsMngr);
			if (xmlNode != null)
			{
				m_catDescr = xmlNode.InnerText;
			}
			CatalogConfigEntry catalogEntry = CatalogsConfig.GetCatalogEntry(m_catID);
			m_catTested = catalogEntry != null;
			m_AllStars.Clear();
			foreach (XmlNode item in resourceNode.SelectNodes("./vt:TABLE", nsMngr)!)
			{
				m_epochRA = null;
				m_epochDE = null;
				m_equinox = null;
				m_referenceSystem = null;
				m_epochRAIdx = -1;
				m_epochDEIdx = -1;
				m_epochErrorRAIdx = -1;
				m_epochErrorDEIdx = -1;
				m_idIdxMulti = null;
				m_raIdxCat = -1;
				m_deIdxCat = -1;
				m_epochIdx = -1;
				m_MbIdx = -1;
				m_MvIdx = -1;
				m_MrIdx = -1;
				m_RVIdx = -1;
				string value = item.Attributes!["name"]!.Value;
				if ((value.Contains("I/337") && ((!value.Contains("tgas") & !value.Contains("gaia")) || value.Contains("tgasptyc"))) || value.Contains("VI/137") || value.Contains("VI/145"))
				{
					continue;
				}
				xmlNode = item.SelectSingleNode("./vt:DESCRIPTION", nsMngr);
				if (xmlNode != null)
				{
					m_tableDescr = xmlNode.InnerText;
				}
				m_idIdx = GetFieldIndex(item, "ucd", "meta.id;meta.main", out var unitName, out var desc, out m_catDesgn);
				if (m_idIdx == -1)
				{
					m_idIdxMulti = GetFieldIndexes(item, "ucd", "meta.id.part;meta.main").ToArray();
					if (m_idIdxMulti.Length != 0)
					{
						m_idIdx = m_idIdxMulti[0];
						m_catDesgn = catalogEntry.AttName_ID;
					}
				}
				if (m_idIdx == -1)
				{
					m_idIdx = GetFieldIndex(item, "ucd", "meta.record", out unitName, out desc, out m_catDesgn);
					m_catDesgn = string.Empty;
				}
				if (catalogEntry.AttName_EPOCH != null)
				{
					m_epochRAIdx = GetFieldIndex(item, "name", catalogEntry.FirstEpochAttName, out m_EpochUnitRA, out m_EpochDescRA);
					if (catalogEntry.SecondEpochAttName != null)
					{
						m_epochDEIdx = GetFieldIndex(item, "name", catalogEntry.SecondEpochAttName, out m_EpochUnitDE, out m_EpochDescDE);
					}
					else
					{
						m_epochDEIdx = m_epochRAIdx;
						m_EpochUnitDE = m_EpochUnitRA;
						m_EpochDescDE = m_EpochDescRA;
					}
				}
				string fieldName2;
				string fieldName;
				string refName;
				string refName2 = (fieldName2 = (refName = (fieldName = null)));
				if (catalogEntry.AttName_RA != null)
				{
					m_raIdxCat = GetFieldIndex(item, "name", catalogEntry.AttName_RA, out m_raUnitCat, out m_raDescCat, out fieldName2, out refName2);
				}
				if (m_raIdxCat == -1)
				{
					m_raIdxCat = GetFieldIndex(item, "ucd", "POS_EQ_RA_MAIN", out m_raUnitCat, out m_raDescCat, out fieldName2, out refName2);
				}
				if (m_raIdxCat == -1)
				{
					m_raIdxCat = GetFieldIndex(item, "ucd", "pos.eq.ra", out m_raUnitCat, out m_raDescCat, out fieldName2, out refName2);
				}
				List<int> list = ((catalogEntry.AttName_RA != null) ? GetFieldIndexes(item, "name", catalogEntry.AttName_RA, out var unitNames, out var descs, out var fieldNames, out var refNames) : GetFieldIndexes(item, "ucd", "pos.eq.ra;meta.main", out unitNames, out descs, out fieldNames, out refNames));
				for (int i = 0; i < list.Count; i++)
				{
					if (fieldNames[i] == "_RAJ2000")
					{
						m_raIdxJ2000 = list[i];
						m_raUnitJ2000 = unitNames[i];
						m_raDescJ2000 = descs[i];
					}
					else
					{
						m_raIdxCat = list[i];
						m_raUnitCat = unitNames[i];
						m_raDescCat = descs[i];
						fieldName2 = fieldNames[i];
						refName2 = refNames[i];
					}
				}
				if (m_raIdxCat != -1 && string.IsNullOrEmpty(refName2))
				{
					Console.WriteLine("WARNING: NO ALPHA REF -> " + fieldName2 + " : " + m_catID + " (" + m_catDescr + ")");
				}
				if (catalogEntry.AttName_DE != null)
				{
					m_deIdxCat = GetFieldIndex(item, "name", catalogEntry.AttName_DE, out m_deUnitCat, out m_deDescCat, out fieldName, out refName);
				}
				if (m_deIdxCat == -1)
				{
					m_deIdxCat = GetFieldIndex(item, "ucd", "POS_EQ_DEC_MAIN", out m_deUnitCat, out m_deDescCat, out fieldName, out refName);
				}
				if (m_deIdxCat == -1)
				{
					m_deIdxCat = GetFieldIndex(item, "ucd", "pos.eq.dec", out m_deUnitCat, out m_deDescCat, out fieldName, out refName);
				}
				list = ((catalogEntry.AttName_DE != null) ? GetFieldIndexes(item, "name", catalogEntry.AttName_DE, out unitNames, out descs, out fieldNames, out refNames) : GetFieldIndexes(item, "ucd", "pos.eq.dec;meta.main", out unitNames, out descs, out fieldNames, out refNames));
				for (int j = 0; j < list.Count; j++)
				{
					if (fieldNames[j] == "_DEJ2000")
					{
						m_deIdxJ2000 = list[j];
						m_deUnitJ2000 = unitNames[j];
						m_deDescJ2000 = descs[j];
					}
					else
					{
						m_deIdxCat = list[j];
						m_deUnitCat = unitNames[j];
						m_deDescCat = descs[j];
						fieldName = fieldNames[j];
						refName = refNames[j];
					}
				}
				if (!string.Equals(refName, refName2))
				{
					Console.WriteLine("WARNING: DIFF RA/DEC REFS -> " + m_catID + " (" + m_catDescr + ")");
				}
				if (refName2 != null)
				{
					XmlNode xmlNode3 = resourceNode.OwnerDocument.SelectSingleNode($"//vt:COOSYS[@ID='{refName2}']", nsMngr);
					if (xmlNode3 != null)
					{
						if (xmlNode3.Attributes!["equinox"] != null)
						{
							m_equinox = xmlNode3.Attributes!["equinox"]!.Value;
						}
						if (xmlNode3.Attributes!["system"] != null)
						{
							m_referenceSystem = xmlNode3.Attributes!["system"]!.Value;
						}
						if (m_epochRAIdx == -1 && xmlNode3.Attributes!["epoch"] != null)
						{
							m_epochRA = (m_epochDE = xmlNode3.Attributes!["epoch"]!.Value);
							m_EpochDescRA = (m_EpochDescDE = "Epoch specified in the COORSYS data");
							m_EpochUnitRA = (m_EpochUnitDE = "yr");
						}
					}
					else
					{
						Console.WriteLine("Cannot find COOSYS " + refName2);
					}
					if (m_epochRAIdx == -1)
					{
						if (m_epochIdx == -1)
						{
							m_epochIdx = GetFieldIndex(item, "ucd", "time.epoch", out m_EpochUnit, out refName_Epoch);
						}
						if (m_epochIdx == -1)
						{
							m_epochIdx = GetFieldIndex(item, "ucd", "TIME_EPOCH", out m_EpochUnit, out refName_Epoch);
						}
					}
				}
				if ((catalogEntry.Flags & CatFlags.FixedFK5_J2000_2000) == CatFlags.FixedFK5_J2000_2000)
				{
					m_equinox = "J2000";
					m_referenceSystem = "eq_FK5";
					m_epochRA = (m_epochDE = "2000");
					m_EpochDescRA = (m_EpochDescDE = "Fixed epoch 2000.0 specified by the software configuration");
					m_EpochUnitRA = (m_EpochUnitDE = "yr");
					refName2 = "!fixed!";
					refName = "!fixed!";
				}
				if (catalogEntry.AttName_ERRRA != null)
				{
					m_raErrIdx = GetFieldIndex(item, "name", catalogEntry.AttName_ERRRA, out m_raErrUnit, out m_raErrDesc);
				}
				else
				{
					m_raErrIdx = GetFieldIndex(item, "ucd", "stat.error;pos.eq.ra", out m_raErrUnit, out m_raErrDesc);
					if (m_raErrIdx == -1)
					{
						m_raErrIdx = GetFieldIndex(item, "name", "e_RAmdeg", out m_raErrUnit, out m_raErrDesc);
					}
					if (m_raErrIdx == -1)
					{
						m_raErrIdx = GetFieldIndex(item, "name", "e_RA*", out m_raErrUnit, out m_raErrDesc);
					}
					if (m_raErrIdx == -1)
					{
						m_raErrIdx = GetFieldIndex(item, "name", "errMaj", out m_raErrUnit, out m_raErrDesc);
						m_radecErrPAIdx = GetFieldIndex(item, "name", "errPA", out m_raErrUnit, out m_raErrDesc);
					}
				}
				if (catalogEntry.AttName_ERRDE != null)
				{
					m_deErrIdx = GetFieldIndex(item, "name", catalogEntry.AttName_ERRDE, out m_deErrUnit, out m_deErrDesc);
				}
				else
				{
					m_deErrIdx = GetFieldIndex(item, "ucd", "stat.error;pos.eq.dec", out m_deErrUnit, out m_deErrDesc);
					if (m_deErrIdx == -1)
					{
						m_deErrIdx = GetFieldIndex(item, "name", "e_DEmdeg", out m_deErrUnit, out m_deErrDesc);
					}
					if (m_deErrIdx == -1)
					{
						m_deErrIdx = GetFieldIndex(item, "name", "e_DEs", out m_deErrUnit, out m_deErrDesc);
					}
					if (m_deErrIdx == -1)
					{
						m_deErrIdx = GetFieldIndex(item, "name", "errMin", out m_deErrUnit, out m_deErrDesc);
					}
				}
				string fieldName3;
				string refName3;
				if (catalogEntry.AttName_PMRA != null)
				{
					m_pmraIdx = GetFieldIndex(item, "name", catalogEntry.AttName_PMRA, out m_pmraUnit, out m_pmraDesc, out fieldName3, out refName3);
				}
				else
				{
					m_pmraIdx = GetFieldIndex(item, "ucd", "pos.pm;pos.eq.ra", out m_pmraUnit, out m_pmraDesc, out fieldName3, out refName3);
				}
				if (m_pmraIdx == -1)
				{
					m_pmraIdx = GetFieldIndex(item, "ucd", "POS_EQ_PMRA", out m_pmraUnit, out m_pmraDesc, out fieldName3, out refName3);
				}
				string fieldName4;
				string refName4;
				if (catalogEntry.AttName_PMDE != null)
				{
					m_pmdeIdx = GetFieldIndex(item, "name", catalogEntry.AttName_PMDE, out m_pmdeUnit, out m_pmdeDesc, out fieldName4, out refName4);
				}
				else
				{
					m_pmdeIdx = GetFieldIndex(item, "ucd", "pos.pm;pos.eq.dec", out m_pmdeUnit, out m_pmdeDesc, out fieldName4, out refName4);
				}
				if (m_pmdeIdx == -1)
				{
					m_pmdeIdx = GetFieldIndex(item, "ucd", "POS_EQ_PMDEC", out m_pmdeUnit, out m_pmdeDesc, out fieldName4, out refName4);
				}
				if (catalogEntry.AttName_ERRPMRA != null)
				{
					m_pmraErrIdx = GetFieldIndex(item, "name", catalogEntry.AttName_ERRPMRA, out m_pmraErrUnit, out m_pmraErrDesc);
				}
				else
				{
					m_pmraErrIdx = GetFieldIndex(item, "ucd", "stat.error;pos.pm;pos.eq.ra", out m_pmraErrUnit, out m_pmraErrDesc);
					if (m_pmraErrIdx == -1)
					{
						m_pmraErrIdx = GetFieldIndex(item, "name", "e_pmRA", out m_pmraErrUnit, out m_pmraErrDesc);
					}
					if (m_pmraErrIdx == -1)
					{
						m_pmraErrIdx = GetFieldIndex(item, "name", "e_pmRA*", out m_pmraErrUnit, out m_pmraErrDesc);
					}
				}
				if (catalogEntry.AttName_ERRPMDE != null)
				{
					m_pmdeErrIdx = GetFieldIndex(item, "name", catalogEntry.AttName_ERRPMDE, out m_pmdeErrUnit, out m_pmdeErrDesc);
				}
				else
				{
					m_pmdeErrIdx = GetFieldIndex(item, "ucd", "stat.error;pos.pm;pos.eq.dec", out m_pmdeErrUnit, out m_pmdeErrDesc);
					if (m_pmdeErrIdx == -1)
					{
						m_pmdeErrIdx = GetFieldIndex(item, "name", "e_pmDE", out m_pmdeErrUnit, out m_pmdeErrDesc);
					}
				}
				if (catalogEntry.AttName_RUWE != null)
				{
					m_ruweIdx = GetFieldIndex(item, "name", catalogEntry.AttName_RUWE, out m_ruweUnit, out m_ruweDesc);
				}
				else
				{
					m_ruweIdx = GetFieldIndex(item, "ucd", "stat.error", out m_ruweUnit, out m_ruweDesc);
					if (m_ruweIdx == -1)
					{
						m_ruweIdx = GetFieldIndex(item, "name", "RUWE", out m_ruweUnit, out m_ruweDesc);
					}
				}
				if (m_MbIdx == -1)
				{
					m_MbIdx = GetFieldIndex(item, "ucd", "phot.mag;em.opt.B", out m_MbUnit, out refName_Mb);
				}
				if (m_MvIdx == -1)
				{
					m_MvIdx = GetFieldIndex(item, "ucd", "phot.mag;em.opt.V", out m_MvUnit, out refName_Mv);
				}
				if (m_MvIdx == -1)
				{
					m_MvIdx = GetFieldIndex(item, "ucd", "phot.mag;em.opt", out m_MvUnit, out refName_Mv);
				}
				if (m_MrIdx == -1)
				{
					m_MrIdx = GetFieldIndex(item, "ucd", "phot.mag;em.opt.R", out m_MrUnit, out refName_Mr);
				}
				if (m_RVIdx == -1)
				{
					m_MbIdx = GetFieldIndex(item, "ucd", "RV", out m_RVunit, out refName_RV);
				}
				if (m_epochErrorRAIdx == -1)
				{
					m_epochErrorRAIdx = GetFieldIndex(item, "name", "TRA", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorRAIdx == -1)
				{
					m_epochErrorRAIdx = GetFieldIndex(item, "name", "EpRA", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorRAIdx == -1)
				{
					m_epochErrorRAIdx = GetFieldIndex(item, "name", "epRA", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorRAIdx == -1)
				{
					m_epochErrorRAIdx = GetFieldIndex(item, "name", "EpRAm", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorRAIdx == -1)
				{
					m_epochErrorRAIdx = GetFieldIndex(item, "ucd", "time.epoch;obs", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorDEIdx == -1)
				{
					m_epochErrorDEIdx = GetFieldIndex(item, "name", "TDE", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorDEIdx == -1)
				{
					m_epochErrorDEIdx = GetFieldIndex(item, "name", "EpDE", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorDEIdx == -1)
				{
					m_epochErrorDEIdx = GetFieldIndex(item, "name", "epDE", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorDEIdx == -1)
				{
					m_epochErrorDEIdx = GetFieldIndex(item, "name", "EpDEm", out m_EpochErrorUnit, out refName_EpochError);
				}
				if (m_epochErrorDEIdx == -1)
				{
					m_epochErrorDEIdx = GetFieldIndex(item, "ucd", "time.epoch;obs", out m_EpochErrorUnit, out refName_EpochError);
				}
				foreach (XmlNode item2 in item.SelectNodes("./vt:DATA/vt:TABLEDATA/vt:TR", nsMngr)!)
				{
					int num = 1;
					if (m_catID == "I/264")
					{
						num = 2;
					}
					for (int k = 0; k < num; k++)
					{
						StarEntry starEntry = new StarEntry();
						starEntry.CatalogName = m_catDescr;
						starEntry.DataSetName = m_tableDescr;
						starEntry.CatalogId = m_catID;
						starEntry.CatalogOutputIsTested = m_catTested;
						starEntry.unit_RA = m_raUnitCat;
						starEntry.unit_DEC = m_deUnitCat;
						starEntry.unit_pmRA = m_pmraUnit;
						starEntry.unit_pmDEC = m_pmdeUnit;
						starEntry.unit_e_RA = m_raErrUnit;
						starEntry.unit_e_DEC = m_deErrUnit;
						starEntry.unit_e_pmRA = m_pmraErrUnit;
						starEntry.unit_e_pmDEC = m_pmdeErrUnit;
						starEntry.desc_RACat = m_raDescCat;
						starEntry.desc_DECat = m_deDescCat;
						starEntry.desc_RAJ2000_2000 = m_raDescJ2000;
						starEntry.desc_DEJ2000_2000 = m_deDescJ2000;
						starEntry.desc_pmRA = m_pmraDesc;
						starEntry.desc_pmDEC = m_pmdeDesc;
						starEntry.desc_e_RA = m_raErrDesc;
						starEntry.desc_e_DEC = m_deErrDesc;
						starEntry.desc_e_pmRA = m_pmraErrDesc;
						starEntry.desc_e_pmDEC = m_pmdeErrDesc;
						starEntry.desc_EpochRA = m_EpochDescRA;
						starEntry.unit_EpochRA = m_EpochUnitRA;
						starEntry.desc_EpochDE = m_EpochDescDE;
						starEntry.unit_EpochDE = m_EpochUnitDE;
						if (m_idIdx != -1)
						{
							if (m_idIdxMulti == null)
							{
								starEntry.StarName = m_catDesgn + " " + item2.ChildNodes[m_idIdx]!.InnerText;
							}
							else
							{
								StringBuilder stringBuilder = new StringBuilder(m_catDesgn + " ");
								for (int l = 0; l < m_idIdxMulti.Length; l++)
								{
									stringBuilder.Append(item2.ChildNodes[m_idIdxMulti[l]]!.InnerText);
									if (l < m_idIdxMulti.Length - 1)
									{
										stringBuilder.Append("-");
									}
								}
								starEntry.StarName = stringBuilder.ToString();
							}
						}
						if (m_MbIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_MbIdx]!.InnerText))
						{
							starEntry.Mb = GetDoubleValue(item2.ChildNodes[m_MbIdx]!.InnerText, m_MbUnit);
						}
						else
						{
							starEntry.Mb = 0.0;
						}
						if (m_MvIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_MvIdx]!.InnerText))
						{
							starEntry.Mv = GetDoubleValue(item2.ChildNodes[m_MvIdx]!.InnerText, m_MvUnit);
						}
						else
						{
							starEntry.Mv = 0.0;
						}
						if (m_MrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_MrIdx]!.InnerText))
						{
							starEntry.Mr = GetDoubleValue(item2.ChildNodes[m_MrIdx]!.InnerText, m_MrUnit);
						}
						else
						{
							starEntry.Mr = 0.0;
						}
						if (m_RVIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_RVIdx]!.InnerText))
						{
							starEntry.RV = GetDoubleValue(item2.ChildNodes[m_RVIdx]!.InnerText, m_MvUnit);
						}
						else
						{
							starEntry.RV = 0.0;
						}
						if (((m_epochErrorRAIdx != -1) & (m_catDesgn != "2MASS")) && !string.Empty.Equals(item2.ChildNodes[m_epochErrorRAIdx]!.InnerText))
						{
							starEntry.EpochErrorRA = GetDoubleValue(item2.ChildNodes[m_epochErrorRAIdx]!.InnerText, m_EpochErrorUnit);
						}
						else
						{
							starEntry.EpochErrorRA = 0.0;
						}
						if (((m_epochErrorDEIdx != -1) & (m_catDesgn != "2MASS")) && !string.Empty.Equals(item2.ChildNodes[m_epochErrorDEIdx]!.InnerText))
						{
							starEntry.EpochErrorDE = GetDoubleValue(item2.ChildNodes[m_epochErrorDEIdx]!.InnerText, m_EpochErrorUnit);
						}
						else
						{
							starEntry.EpochErrorDE = 0.0;
						}
						string text = m_epochRA;
						if (m_epochRAIdx != -1 && ((catalogEntry != null && !string.IsNullOrEmpty(catalogEntry.AttName_EPOCH)) || string.IsNullOrEmpty(text)))
						{
							text = item2.ChildNodes[m_epochRAIdx]!.InnerText;
						}
						string text2 = m_epochDE;
						if (m_epochDEIdx != -1 && ((catalogEntry != null && !string.IsNullOrEmpty(catalogEntry.AttName_EPOCH)) || string.IsNullOrEmpty(text2)))
						{
							text2 = item2.ChildNodes[m_epochDEIdx]!.InnerText;
						}
						if (m_epochIdx != -1)
						{
							if (!double.TryParse(item2.ChildNodes[m_epochIdx]!.InnerText, out var result))
							{
								result = 2415020.0;
							}
							m_EpochUnitRA = (m_EpochUnitDE = "yr");
						}
						if (m_raIdxCat != -1 && !string.Empty.Equals(item2.ChildNodes[m_raIdxCat]!.InnerText))
						{
							starEntry.RACat = NormalizeRAValue(GetDoubleValue(item2.ChildNodes[m_raIdxCat]!.InnerText, m_raUnitCat), m_raUnitCat);
						}
						else
						{
							starEntry.RACat = double.NaN;
						}
						if (m_deIdxCat != -1 && !string.Empty.Equals(item2.ChildNodes[m_deIdxCat]!.InnerText))
						{
							starEntry.DECat = NormalizeDEValue(GetDoubleValue(item2.ChildNodes[m_deIdxCat]!.InnerText, m_deUnitCat), m_deUnitCat);
						}
						else
						{
							starEntry.DECat = double.NaN;
						}
						if (m_raIdxJ2000 != -1 && !string.Empty.Equals(item2.ChildNodes[m_raIdxJ2000]!.InnerText))
						{
							starEntry.RAJ2000_2000 = NormalizeRAValue(GetDoubleValue(item2.ChildNodes[m_raIdxJ2000]!.InnerText, m_raUnitJ2000), m_raUnitJ2000);
						}
						else
						{
							starEntry.RAJ2000_2000 = double.NaN;
						}
						if (m_deIdxJ2000 != -1 && !string.Empty.Equals(item2.ChildNodes[m_deIdxJ2000]!.InnerText))
						{
							starEntry.DEJ2000_2000 = NormalizeDEValue(GetDoubleValue(item2.ChildNodes[m_deIdxJ2000]!.InnerText, m_deUnitJ2000), m_deUnitJ2000);
						}
						else
						{
							starEntry.DEJ2000_2000 = double.NaN;
						}
						if (catalogEntry.CatalogId == "*")
						{
							if (m_raErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_raErrIdx]!.InnerText))
							{
								starEntry.e_RA = double.Parse(item2.ChildNodes[m_raErrIdx]!.InnerText, CultureInfo.InvariantCulture);
							}
							else
							{
								starEntry.e_RA = double.NaN;
							}
							if (m_deErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_deErrIdx]!.InnerText))
							{
								starEntry.e_DEC = double.Parse(item2.ChildNodes[m_deErrIdx]!.InnerText, CultureInfo.InvariantCulture);
							}
							else
							{
								starEntry.e_DEC = double.NaN;
							}
							if (m_radecErrPAIdx != -1 && (!double.IsNaN(starEntry.e_RA) & !double.IsNaN(starEntry.e_DEC)))
							{
								double num2 = double.Parse(item2.ChildNodes[m_radecErrPAIdx]!.InnerText, CultureInfo.InvariantCulture);
								num2 /= 57.295779;
								double e_RA = Math.Sqrt(Math.Pow(starEntry.e_RA * Math.Sin(num2), 2.0) + Math.Pow(starEntry.e_DEC * Math.Cos(num2), 2.0));
								double e_DEC = Math.Sqrt(Math.Pow(starEntry.e_RA * Math.Cos(num2), 2.0) + Math.Pow(starEntry.e_DEC * Math.Sin(num2), 2.0));
								starEntry.e_RA = e_RA;
								starEntry.e_DEC = e_DEC;
							}
						}
						else
						{
							if (m_raErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_raErrIdx]!.InnerText))
							{
								starEntry.e_RA = NormalizeRAERRValue(double.Parse(item2.ChildNodes[m_raErrIdx]!.InnerText, CultureInfo.InvariantCulture), m_raErrUnit);
							}
							else
							{
								starEntry.e_RA = double.NaN;
							}
							if (m_deErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_deErrIdx]!.InnerText))
							{
								starEntry.e_DEC = NormalizeDEERRValue(double.Parse(item2.ChildNodes[m_deErrIdx]!.InnerText, CultureInfo.InvariantCulture), m_deErrUnit);
							}
							else
							{
								starEntry.e_DEC = double.NaN;
							}
						}
						if (m_pmraIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_pmraIdx]!.InnerText))
						{
							starEntry.pmRA = NormalizePMRAValue(double.Parse(item2.ChildNodes[m_pmraIdx]!.InnerText, CultureInfo.InvariantCulture), m_pmraUnit);
							if (((catalogEntry.Flags & CatFlags.PmRecCosDec) != 0) | (catalogEntry.AttName_ID == "HIP ") | (catalogEntry.CatalogId == "I/264") | (catalogEntry.CatalogId == "I/284"))
							{
								starEntry.pmRA = starEntry.pmRA / 15.0 / Math.Cos(starEntry.DECat * Math.PI / 180.0);
							}
						}
						else
						{
							starEntry.pmRA = double.NaN;
						}
						if (m_pmdeIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_pmdeIdx]!.InnerText))
						{
							starEntry.pmDEC = NormalizePMDEValue(double.Parse(item2.ChildNodes[m_pmdeIdx]!.InnerText, CultureInfo.InvariantCulture), m_pmdeUnit);
						}
						else
						{
							starEntry.pmDEC = double.NaN;
						}
						if (m_pmraErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_pmraErrIdx]!.InnerText))
						{
							starEntry.e_pmRA = NormalizePMRAValue(double.Parse(item2.ChildNodes[m_pmraErrIdx]!.InnerText, CultureInfo.InvariantCulture), m_pmraErrUnit);
						}
						else
						{
							starEntry.e_pmRA = double.NaN;
						}
						if (m_pmdeErrIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_pmdeErrIdx]!.InnerText))
						{
							starEntry.e_pmDEC = NormalizePMDEValue(double.Parse(item2.ChildNodes[m_pmdeErrIdx]!.InnerText, CultureInfo.InvariantCulture), m_pmdeErrUnit);
						}
						else
						{
							starEntry.e_pmDEC = double.NaN;
						}
						starEntry.CatalogParserInfo = catalogEntry;
						starEntry.System = StringToReferenceSystem(m_referenceSystem);
						if (catalogEntry != null && starEntry.System == ReferenceSystem.FK5BA && catalogEntry.System == ReferenceSystem.ICRS)
						{
							starEntry.System = ReferenceSystem.ICRS;
						}
						try
						{
							starEntry.Equinox = NormalizeEquinox(m_equinox);
						}
						catch (Exception ex)
						{
							Trace.WriteLine(ex.ToString());
						}
						try
						{
							starEntry.EpochRA = NormalizeEpoch(catalogEntry, text, m_EpochUnitRA);
						}
						catch (Exception ex2)
						{
							starEntry.EpochRA = double.NaN;
							Trace.WriteLine(ex2.ToString());
						}
						try
						{
							starEntry.EpochDE = NormalizeEpoch(catalogEntry, text2, m_EpochUnitDE);
						}
						catch (Exception ex3)
						{
							starEntry.EpochDE = double.NaN;
							Trace.WriteLine(ex3.ToString());
						}
						if ((starEntry.EpochErrorRA == 0.0) & (starEntry.EpochRA > 0.0))
						{
							starEntry.EpochErrorRA = starEntry.EpochRA;
						}
						if ((starEntry.EpochErrorDE == 0.0) & (starEntry.EpochDE > 0.0))
						{
							starEntry.EpochErrorDE = starEntry.EpochDE;
						}
						starEntry.flag_Double = ".";
						starEntry.flag_NumCats = "..";
						starEntry.flag_Other = ".";
						starEntry.flag_Variable = ".";
						starEntry.Parallax = 0.0;
						starEntry.e_Parallax = 0.0;
						if (catalogEntry.CatalogId == "I/311")
						{
							m_ParallaxIdx = GetFieldIndex(item, "name", "Plx", out m_FlagUnit, out refName_Flag);
							if (m_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_ParallaxIdx]!.InnerText))
							{
								starEntry.Parallax = NormalizeParallax(item2.ChildNodes[m_ParallaxIdx]!.InnerText);
							}
							m_e_ParallaxIdx = GetFieldIndex(item, "name", "e_Plx", out m_FlagUnit, out refName_Flag);
							if (m_e_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText))
							{
								starEntry.e_Parallax = NormalizeParallax(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText);
							}
							m_FlagIdx = GetFieldIndex(item, "name", "Sn", out m_FlagUnit, out refName_Flag);
							string text3 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "So", out m_FlagUnit, out refName_Flag);
							if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
							{
								_ = item2.ChildNodes[m_FlagIdx]!.InnerText;
							}
							m_FlagIdx = GetFieldIndex(item, "name", "n_HIP", out m_FlagUnit, out refName_Flag);
							if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
							{
								_ = item2.ChildNodes[m_FlagIdx]!.InnerText;
							}
							m_FlagIdx = GetFieldIndex(item, "name", "Nc", out m_FlagUnit, out refName_Flag);
							string text4 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "" : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (text3.EndsWith("5") & !text3.StartsWith("8"))
							{
								starEntry.flag_Other = " ";
							}
							else
							{
								starEntry.flag_Other = "*";
							}
							if (text3.StartsWith("2"))
							{
								starEntry.flag_Variable = "v";
							}
							else
							{
								starEntry.flag_Variable = " ";
							}
							if ((text4 == "") | (text4 == "1") | !(text3.StartsWith("1") | text3.StartsWith("4")))
							{
								starEntry.flag_Double = " ";
							}
							else
							{
								starEntry.flag_Double = "d";
							}
						}
						else if (catalogEntry.CatalogId == "I/264")
						{
							m_ParallaxIdx = GetFieldIndex(item, "name", "plx", out m_FlagUnit, out refName_Flag);
							if (m_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_ParallaxIdx]!.InnerText))
							{
								starEntry.Parallax = NormalizeParallax(item2.ChildNodes[m_ParallaxIdx]!.InnerText);
							}
							m_e_ParallaxIdx = GetFieldIndex(item, "name", "e_plx", out m_FlagUnit, out refName_Flag);
							if (m_e_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText))
							{
								starEntry.e_Parallax = NormalizeParallax(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText);
							}
							m_FlagIdx = GetFieldIndex(item, "name", "f_Vmag", out m_FlagUnit, out refName_Flag);
							string value2 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Kbin1", out m_FlagUnit, out refName_Flag);
							string text5 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "Kbin2", out m_FlagUnit, out refName_Flag);
							string text6 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if ((text6 == "2") | "2346".Contains(text5.Substring(1, 1)))
							{
								starEntry.flag_Double = "d";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
							if ("789".Contains(text5.Substring(1, 1)))
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if ("12".Contains(value2))
							{
								starEntry.flag_Variable = "v";
							}
							else
							{
								starEntry.flag_Variable = " ";
							}
						}
						else if (catalogEntry.CatalogId == "I/329")
						{
							int result2 = -1;
							m_FlagIdx = GetFieldIndex(item, "name", "Niu", out m_FlagUnit, out refName_Flag);
							string text7 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "1" : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (text7.Length < 3)
							{
								starEntry.flag_NumCats = text7.PadLeft(2);
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
							if (!int.TryParse(text7, out result2))
							{
								result2 = 1;
							}
							if (result2 > 19)
							{
								starEntry.e_DEC = starEntry.e_RA;
							}
							else
							{
								starEntry.e_RA = starEntry.e_DEC;
							}
							m_FlagIdx = GetFieldIndex(item, "name", "Bmag", out m_FlagUnit, out refName_Flag);
							if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
							{
								starEntry.Mb = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, m_MbUnit);
							}
							else
							{
								starEntry.Mb = 0.0;
							}
							m_FlagIdx = GetFieldIndex(item, "name", "Vmag", out m_FlagUnit, out refName_Flag);
							if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
							{
								starEntry.Mv = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, m_MbUnit);
							}
							else
							{
								m_FlagIdx = GetFieldIndex(item, "name", "f.mag", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.Mv = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, m_MbUnit);
								}
								else
								{
									starEntry.Mv = 0.0;
								}
							}
							m_FlagIdx = GetFieldIndex(item, "name", "rmag", out m_FlagUnit, out refName_Flag);
							if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
							{
								starEntry.Mr = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, m_MbUnit);
							}
							else
							{
								starEntry.Mr = 0.0;
							}
						}
						else if (catalogEntry.CatalogId == "I/322A")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "db", out m_FlagUnit, out refName_Flag);
							double num3 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? 0.0 : GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, m_MbUnit));
							if (num3 > 10.0)
							{
								starEntry.flag_Double = "d";
							}
							else if (num3 > 2.0)
							{
								starEntry.flag_Double = "*";
							}
							else if (num3 == 1.0)
							{
								starEntry.flag_Double = "?";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
							m_FlagIdx = GetFieldIndex(item, "name", "Nc", out m_FlagUnit, out refName_Flag);
							string text8 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if (text8.Length < 3)
							{
								starEntry.flag_NumCats = text8;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/315")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "ot", out m_FlagUnit, out refName_Flag);
							string text9 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "db", out m_FlagUnit, out refName_Flag);
							string value3 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Cu", out m_FlagUnit, out refName_Flag);
							string text10 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if ("23456".Contains(value3))
							{
								starEntry.flag_Double = "d";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
							if ("17".Contains(value3) | !((text9 == "0") | (text9 == "1")))
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text10.Length < 3)
							{
								starEntry.flag_NumCats = text10;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/289")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Nc", out m_FlagUnit, out refName_Flag);
							string text11 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if (text11.Length < 3)
							{
								starEntry.flag_NumCats = text11;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/259")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "CCDM", out m_FlagUnit, out refName_Flag);
							string text12 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "" : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "posflg", out m_FlagUnit, out refName_Flag);
							string value4 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "pflag", out m_FlagUnit, out refName_Flag);
							string text13 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if ((text12.Trim().Length > 0) | "PD".Contains(value4) | (text13 == "P"))
							{
								starEntry.flag_Double = "d";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
							if (text13 == "X")
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
						}
						else if (starEntry.CatalogId == "I/304")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Na", out m_FlagUnit, out refName_Flag);
							string text14 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "f_CMC14", out m_FlagUnit, out refName_Flag);
							string text15 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "" : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (text15 == "*")
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text14.Length < 3)
							{
								starEntry.flag_NumCats = text14;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (starEntry.CatalogId == "I/327")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Na", out m_FlagUnit, out refName_Flag);
							string text16 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "f_CMC15", out m_FlagUnit, out refName_Flag);
							string text17 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "" : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (text17 == "*")
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text16.Length < 3)
							{
								starEntry.flag_NumCats = text16;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/312")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "sub", out m_FlagUnit, out refName_Flag);
							string flag_Other = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Nobs", out m_FlagUnit, out refName_Flag);
							string text18 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if (text18.Length < 3)
							{
								starEntry.flag_NumCats = text18;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
							starEntry.flag_Other = flag_Other;
						}
						else if (catalogEntry.CatalogId == "I/317")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "fl", out m_FlagUnit, out refName_Flag);
							string s = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!int.TryParse(s, out var result3))
							{
								result3 = 0;
							}
							if ((result3 & 1) > 0)
							{
								starEntry.flag_Other = "?";
							}
							else if ((result3 & 2) > 0 || (result3 & 2) > 0)
							{
								starEntry.flag_Other = "P";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							m_FlagIdx = GetFieldIndex(item, "name", "No", out m_FlagUnit, out refName_Flag);
							string text19 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if (text19.Length < 3)
							{
								starEntry.flag_NumCats = text19;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (starEntry.CatalogId == "II/246")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "dup", out m_FlagUnit, out refName_Flag);
							string text20 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Aflg", out m_FlagUnit, out refName_Flag);
							string text21 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Cflg", out m_FlagUnit, out refName_Flag);
							string value5 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Xflg", out m_FlagUnit, out refName_Flag);
							string value6 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if ("12".Contains(value6) | (text21 == "1") | "pcdsb".Contains(value5))
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text20 == "0")
							{
								starEntry.flag_Double = " ";
							}
							else if (text20 == "1")
							{
								starEntry.flag_Double = "?";
							}
							else
							{
								starEntry.flag_Double = "d";
							}
							m_FlagIdx = GetFieldIndex(item, "name", "JD", out m_FlagUnit, out refName_Flag);
							string s2 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!double.TryParse(s2, out var result4))
							{
								result4 = 2415020.0;
							}
							starEntry.EpochRA = (starEntry.EpochDE = (starEntry.EpochErrorRA = (starEntry.EpochErrorDE = Utilities.BesselianYear(result4))));
						}
						else if (catalogEntry.CatalogId == "I/146")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Npos", out m_FlagUnit, out refName_Flag);
							string text22 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "Flag1", out m_FlagUnit, out refName_Flag);
							string value7 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							m_FlagIdx = GetFieldIndex(item, "name", "Flag2", out m_FlagUnit, out refName_Flag);
							string text23 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if ("PC".Contains(value7))
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text23 == "D")
							{
								starEntry.flag_Double = "d";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
							if (text22.Length < 3)
							{
								starEntry.flag_NumCats = text22;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/284")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Ndet", out m_FlagUnit, out refName_Flag);
							string text24 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							m_FlagIdx = GetFieldIndex(item, "name", "Flags", out m_FlagUnit, out refName_Flag);
							string text25 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (text25 == "s")
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if (text24 == " 0")
							{
								starEntry.flag_NumCats = "..";
							}
							else if (text24.Length < 3)
							{
								starEntry.flag_NumCats = text24;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
						}
						else if (catalogEntry.CatalogId == "I/297")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Xflags", out m_FlagUnit, out refName_Flag);
							string s3 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!int.TryParse(s3, out var result5))
							{
								result5 = 0;
							}
							if ((result5 & 1) > 0 || (result5 & 2) > 0 || (result5 & 0x2000) > 0 || (result5 & 0x10000) > 0)
							{
								starEntry.flag_Other = "*";
							}
							else
							{
								starEntry.flag_Other = " ";
							}
							if ((result5 & 0x4000) > 0 || (result5 & 0x8000) > 0)
							{
								starEntry.flag_Double = "d";
							}
							else
							{
								starEntry.flag_Double = " ";
							}
						}
						else if (starEntry.CatalogId == "II/321")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "nObs", out m_FlagUnit, out refName_Flag);
							string text26 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "  " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(2));
							if (text26.Length < 3)
							{
								starEntry.flag_NumCats = text26;
							}
							else
							{
								starEntry.flag_NumCats = "99";
							}
							m_FlagIdx = GetFieldIndex(item, "name", "rMJD", out m_FlagUnit, out refName_Flag);
							string s4 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!double.TryParse(s4, out var result6))
							{
								result6 = 51545.0;
							}
							starEntry.EpochRA = (starEntry.EpochDE = (starEntry.EpochErrorRA = (starEntry.EpochErrorDE = Utilities.BesselianYear(result6 + 2400000.5))));
							m_FlagIdx = GetFieldIndex(item, "name", "posErr", out m_FlagUnit, out refName_Flag);
							string s5 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!double.TryParse(s5, out var result7))
							{
								result7 = 0.0;
							}
							starEntry.e_RA = (starEntry.e_DEC = result7);
							m_FlagIdx = GetFieldIndex(item, "name", "a10point", out m_FlagUnit, out refName_Flag);
							string text27 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? "?" : item2.ChildNodes[m_FlagIdx]!.InnerText.Trim());
							if (text27 == "1")
							{
								starEntry.flag_Other = " ";
							}
							else
							{
								starEntry.flag_Other = "?";
							}
						}
						else if (starEntry.CatalogId == "II/316")
						{
							m_FlagIdx = GetFieldIndex(item, "name", "Epoch", out m_FlagUnit, out refName_Flag);
							string s6 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText);
							if (!double.TryParse(s6, out var result8))
							{
								result8 = 2000.0;
							}
							starEntry.EpochRA = (starEntry.EpochDE = (starEntry.EpochErrorRA = (starEntry.EpochErrorDE = result8)));
							m_FlagIdx = GetFieldIndex(item, "name", "p*", out m_FlagUnit, out refName_Flag);
							string s7 = ((m_FlagIdx == -1 || string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText)) ? " " : item2.ChildNodes[m_FlagIdx]!.InnerText.PadLeft(1));
							if (!double.TryParse(s7, out var result9))
							{
								result9 = 0.0;
							}
							if (result9 >= 0.9)
							{
								starEntry.flag_Other = " ";
							}
							else
							{
								starEntry.flag_Other = "?";
							}
						}
						else if ((starEntry.CatalogId == "I/337") | (starEntry.CatalogId == "I/345") | (starEntry.CatalogId == "I/350") | (starEntry.CatalogId == "I/355"))
						{
							int fieldIndex = GetFieldIndex(item, "name", "HIP", out m_FlagUnit, out refName_Flag);
							int fieldIndex2 = GetFieldIndex(item, "name", "TYC2", out m_FlagUnit, out refName_Flag);
							if (fieldIndex != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex]!.InnerText))
							{
								starEntry.StarName = "TGAS HIP " + item2.ChildNodes[fieldIndex]!.InnerText;
							}
							else if (fieldIndex2 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex2]!.InnerText))
							{
								starEntry.StarName = "TGAS Tyc2 " + item2.ChildNodes[fieldIndex2]!.InnerText;
							}
							else if (starEntry.StarName.Contains("Source"))
							{
								starEntry.StarName = "GaiaSrc" + starEntry.StarName.Substring(7);
							}
							if (starEntry.CatalogId == "I/337")
							{
								m_FlagIdx = GetFieldIndex(item, "name", "<Gmag>", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.Mv = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, "mag");
								}
								else
								{
									starEntry.Mv = 0.0;
								}
							}
							else
							{
								m_FlagIdx = GetFieldIndex(item, "name", "Gmag", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.Mv = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, "mag");
								}
								else
								{
									starEntry.Mv = 0.0;
								}
								m_FlagIdx = GetFieldIndex(item, "name", "BPmag", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.Mb = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, "mag");
								}
								else
								{
									starEntry.Mb = 0.0;
								}
								m_FlagIdx = GetFieldIndex(item, "name", "RPmag", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.Mr = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, "mag");
								}
								else
								{
									starEntry.Mr = 0.0;
								}
								m_FlagIdx = GetFieldIndex(item, "name", "RV", out m_FlagUnit, out refName_Flag);
								if (m_FlagIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_FlagIdx]!.InnerText))
								{
									starEntry.RV = GetDoubleValue(item2.ChildNodes[m_FlagIdx]!.InnerText, "km/s");
								}
								else
								{
									starEntry.RV = 0.0;
								}
							}
							if ((starEntry.CatalogId == "I/350") | (starEntry.CatalogId == "I/355"))
							{
								m_ruweIdx = GetFieldIndex(item, "name", "RUWE", out m_ruweUnit, out refName_Flag);
								if (m_ruweIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_ruweIdx]!.InnerText))
								{
									starEntry.RUWE = double.Parse(item2.ChildNodes[m_ruweIdx]!.InnerText);
								}
								m_sourceIdx = GetFieldIndex(item, "name", "Source", out m_sourceUnit, out refName_Flag);
								if (m_ruweIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_sourceIdx]!.InnerText))
								{
									starEntry.SourceID = ulong.Parse(item2.ChildNodes[m_sourceIdx]!.InnerText);
								}
								m_doubleIdx = GetFieldIndex(item, "name", "Dup", out m_doubleUnit, out refName_Flag);
								if (m_doubleIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_doubleIdx]!.InnerText))
								{
									starEntry.DuplicateSource = int.Parse(item2.ChildNodes[m_doubleIdx]!.InnerText);
								}
							}
							m_ParallaxIdx = GetFieldIndex(item, "name", "Plx", out m_FlagUnit, out refName_Flag);
							if (m_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_ParallaxIdx]!.InnerText))
							{
								starEntry.Parallax = NormalizeParallax(item2.ChildNodes[m_ParallaxIdx]!.InnerText);
							}
							m_e_ParallaxIdx = GetFieldIndex(item, "name", "e_Plx", out m_FlagUnit, out refName_Flag);
							if (m_e_ParallaxIdx != -1 && !string.Empty.Equals(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText))
							{
								starEntry.e_Parallax = NormalizeParallax(item2.ChildNodes[m_e_ParallaxIdx]!.InnerText);
							}
						}
						else if (starEntry.CatalogId == "I/339")
						{
							starEntry.StarName = "HSOY " + starEntry.StarName.Substring(5);
						}
						if (m_catID.StartsWith("I/") && m_raIdxCat != -1 && double.IsNaN(starEntry.Equinox))
						{
							Console.WriteLine("NO EQUINOX! " + m_catID + " -> " + m_catDescr);
						}
						if (m_catID.StartsWith("I/") && m_raIdxCat != -1 && double.IsNaN(starEntry.EpochRA))
						{
							Console.WriteLine("NO RA EPOCH! " + m_catID + " -> " + m_catDescr);
						}
						if (m_catID.StartsWith("I/") && m_raIdxCat != -1 && double.IsNaN(starEntry.EpochDE))
						{
							Console.WriteLine("NO DE EPOCH! " + m_catID + " -> " + m_catDescr);
						}
						if (catalogEntry.CatalogId == "I/264")
						{
							if (k > 0)
							{
								starEntry.StarName = starEntry.StarName.Insert(3, "-LTP");
								double num4 = 15.0 * Math.Cos(starEntry.DECat / 57.295779513);
								int fieldIndex3 = GetFieldIndex(item, "name", "DRA*LTP", out m_FlagUnit, out refName_Flag);
								if (fieldIndex3 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex3]!.InnerText))
								{
									starEntry.RACat += NormalizeParallax(item2.ChildNodes[fieldIndex3]!.InnerText) / 3600000.0 / num4;
								}
								int fieldIndex4 = GetFieldIndex(item, "name", "DDE:LTP", out m_FlagUnit, out refName_Flag);
								if (fieldIndex4 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex4]!.InnerText))
								{
									starEntry.DECat += NormalizeParallax(item2.ChildNodes[fieldIndex4]!.InnerText) / 3600000.0;
								}
								int fieldIndex5 = GetFieldIndex(item, "name", "DpmRA*LTP", out m_FlagUnit, out refName_Flag);
								if (fieldIndex5 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex5]!.InnerText))
								{
									starEntry.pmRA += NormalizeParallax(item2.ChildNodes[fieldIndex5]!.InnerText) / 1000.0 / num4;
								}
								int fieldIndex6 = GetFieldIndex(item, "name", "DpmDE:LTP", out m_FlagUnit, out refName_Flag);
								if (fieldIndex6 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex6]!.InnerText))
								{
									starEntry.pmDEC += NormalizeParallax(item2.ChildNodes[fieldIndex6]!.InnerText) / 1000.0;
								}
								if (m_tableDescr.Contains("(I)"))
								{
									int fieldIndex7 = GetFieldIndex(item, "name", "e_RA*LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex7 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex7]!.InnerText))
									{
										starEntry.e_RA = NormalizeParallax(item2.ChildNodes[fieldIndex7]!.InnerText) / 1000.0;
									}
									int fieldIndex8 = GetFieldIndex(item, "name", "e_DE:LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex8 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex8]!.InnerText))
									{
										starEntry.e_DEC = NormalizeParallax(item2.ChildNodes[fieldIndex8]!.InnerText) / 1000.0;
									}
									int fieldIndex9 = GetFieldIndex(item, "name", "e_pmRA*LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex9 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex9]!.InnerText))
									{
										starEntry.e_pmRA = NormalizeParallax(item2.ChildNodes[fieldIndex9]!.InnerText) / 1000.0;
									}
									int fieldIndex10 = GetFieldIndex(item, "name", "e_pmDE:LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex10 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex10]!.InnerText))
									{
										starEntry.e_pmDEC = NormalizeParallax(item2.ChildNodes[fieldIndex10]!.InnerText) / 1000.0;
									}
									int fieldIndex11 = GetFieldIndex(item, "name", "TRALTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex11 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex11]!.InnerText))
									{
										starEntry.EpochErrorRA = NormalizeParallax(item2.ChildNodes[fieldIndex11]!.InnerText);
									}
									int fieldIndex12 = GetFieldIndex(item, "name", "TDELTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex12 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex12]!.InnerText))
									{
										starEntry.EpochErrorDE = NormalizeParallax(item2.ChildNodes[fieldIndex12]!.InnerText);
									}
								}
								else if (m_tableDescr.Contains("(III)"))
								{
									starEntry.StarName += "**";
									int fieldIndex9 = GetFieldIndex(item, "name", "e_DpmRA*LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex9 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex9]!.InnerText))
									{
										starEntry.e_pmRA = NormalizeParallax(item2.ChildNodes[fieldIndex9]!.InnerText) / 1000.0;
									}
									int fieldIndex10 = GetFieldIndex(item, "name", "e_DpmDE:LTP", out m_FlagUnit, out refName_Flag);
									if (fieldIndex10 != -1 && !string.Empty.Equals(item2.ChildNodes[fieldIndex10]!.InnerText))
									{
										starEntry.e_pmDEC = NormalizeParallax(item2.ChildNodes[fieldIndex10]!.InnerText) / 1000.0;
									}
								}
							}
							if (m_tableDescr.Contains("(I)"))
							{
								starEntry.StarName = starEntry.StarName.Insert(3, "(I)");
							}
							if (m_tableDescr.Contains("(III)"))
							{
								starEntry.StarName = starEntry.StarName.Insert(3, "(III)");
							}
						}
						m_AllStars.Add(starEntry);
					}
				}
			}
		}

		private double GetDoubleValue(string strValue, string units)
		{
			if ("\"h:m:s\"" == units)
			{
				string[] array = strValue.Replace(':', ' ').Trim().Split(new char[1] { ' ' });
				double num = 0.0;
				if (array.Length == 3)
				{
					num = (double)int.Parse(array[0]) + (double)int.Parse(array[1]) / 60.0 + double.Parse(array[2], CultureInfo.InvariantCulture) / 3600.0;
				}
				else if (array.Length == 2)
				{
					num = (double)int.Parse(array[0]) + double.Parse(array[1], CultureInfo.InvariantCulture) / 60.0;
				}
				else if (array.Length == 1)
				{
					num = double.Parse(array[0], CultureInfo.InvariantCulture);
				}
				return num * 15.0;
			}
			if ("\"d:m:s\"" == units)
			{
				strValue = strValue.Replace(':', ' ').Trim();
				int num2 = ((!strValue.StartsWith("-")) ? 1 : (-1));
				string[] array2 = strValue.TrimStart('+', '-').Split(new char[1] { ' ' });
				double num3 = 0.0;
				if (array2.Length == 3)
				{
					num3 = (double)int.Parse(array2[0]) + (double)int.Parse(array2[1]) / 60.0 + double.Parse(array2[2], CultureInfo.InvariantCulture) / 3600.0;
				}
				else if (array2.Length == 2)
				{
					num3 = (double)int.Parse(array2[0]) + double.Parse(array2[1], CultureInfo.InvariantCulture) / 60.0;
				}
				else if (array2.Length == 1)
				{
					num3 = double.Parse(array2[0], CultureInfo.InvariantCulture);
				}
				return num3 * (double)num2;
			}
			if ("deg" == units)
			{
				return double.Parse(strValue, CultureInfo.InvariantCulture);
			}
			if ("mag" == units)
			{
				return double.Parse(strValue, CultureInfo.InvariantCulture);
			}
			if ("yr" == units)
			{
				return double.Parse(strValue, CultureInfo.InvariantCulture);
			}
			if ("km/s" == units)
			{
				return double.Parse(strValue, CultureInfo.InvariantCulture);
			}
			return 0.0;
		}

		private double NormalizeRAValue(double rawValue, string unit)
		{
			if ("deg" == unit)
			{
				return rawValue / 15.0;
			}
			if ("\"h:m:s\"" == unit)
			{
				return rawValue / 15.0;
			}
			if ("\"d:m:s\"" == unit)
			{
				return rawValue / 15.0;
			}
			throw new NotSupportedException($"Don't know how to handle RA unit '{unit}'");
		}

		private double NormalizeDEValue(double rawValue, string unit)
		{
			if ("deg" == unit)
			{
				return rawValue;
			}
			if ("\"d:m:s\"" == unit)
			{
				return rawValue;
			}
			throw new NotSupportedException($"Don't know how to handle DE unit '{unit}'");
		}

		private double NormalizeRAERRValue(double rawValue, string unit)
		{
			if ("s" == unit)
			{
				return rawValue;
			}
			if ("ms" == unit)
			{
				return rawValue / 1000.0 * 15.0;
			}
			if ("mas" == unit)
			{
				return rawValue / 1000.0;
			}
			if ("arcsec" == unit)
			{
				return rawValue;
			}
			if ("10mas" == unit)
			{
				return rawValue / 100.0;
			}
			if ("10ms" == unit)
			{
				return rawValue / 100.0 * 15.0;
			}
			if ("cs" == unit)
			{
				return rawValue / 100.0;
			}
			if ("0.01s" == unit)
			{
				return rawValue / 100.0 * 15.0;
			}
			if ("10-1s" == unit)
			{
				return rawValue / 10.0 * 15.0;
			}
			if ("10-2s" == unit)
			{
				return rawValue / 1000.0 * 15.0;
			}
			if ("10-3s" == unit)
			{
				return rawValue / 10000.0 * 15.0;
			}
			if ("10-4s" == unit)
			{
				return rawValue / 100000.0 * 15.0;
			}
			if ("10-5s" == unit)
			{
				return rawValue / 1000000.0 * 15.0;
			}
			if ("10us" == unit)
			{
				return rawValue / 1000000.0 * 15.0;
			}
			if ("deg" == unit)
			{
				return rawValue / 1000.0;
			}
			throw new NotSupportedException($"Don't know how to handle RE sec unit '{unit}'");
		}

		private double NormalizeDEERRValue(double rawValue, string unit)
		{
			if ("s" == unit)
			{
				return rawValue;
			}
			if ("ms" == unit)
			{
				return rawValue / 1000.0;
			}
			if ("mas" == unit)
			{
				return rawValue / 1000.0;
			}
			if ("arcsec" == unit)
			{
				return rawValue;
			}
			if ("carcsec" == unit)
			{
				return rawValue / 100.0;
			}
			if ("marcsec" == unit)
			{
				return rawValue / 1000.0;
			}
			if ("10mas" == unit)
			{
				return rawValue / 100.0;
			}
			if ("0.01arcsec" == unit)
			{
				return rawValue / 100.0;
			}
			if ("cs" == unit)
			{
				return rawValue / 100.0;
			}
			if ("10-1arcsec" == unit)
			{
				return rawValue / 10.0;
			}
			if ("10-2arcsec" == unit)
			{
				return rawValue / 1000.0;
			}
			if ("10-3arcsec" == unit)
			{
				return rawValue / 10000.0;
			}
			if ("10-4arcsec" == unit)
			{
				return rawValue / 100000.0;
			}
			if ("10-5arcsec" == unit)
			{
				return rawValue / 1000000.0;
			}
			if ("deg" == unit)
			{
				return rawValue / 1000.0;
			}
			throw new NotSupportedException($"Don't know how to handle DE arcsec unit '{unit}'");
		}

		private double NormalizePMRAValue(double rawValue, string unit)
		{
			string[] array = unit.Split(new char[1] { '/' });
			double num = NormalizeRAERRValue(rawValue, array[0]);
			if ("yr".Equals(array[1]))
			{
				return num;
			}
			if ("a".Equals(array[1]))
			{
				return num;
			}
			if ("ha".Equals(array[1]))
			{
				return num / 100.0;
			}
			if ("hyr".Equals(array[1]))
			{
				return num / 100.0;
			}
			throw new NotSupportedException($"Don't know how to handle year unit '{array[1]}'");
		}

		private double NormalizePMDEValue(double rawValue, string unit)
		{
			string[] array = unit.Split(new char[1] { '/' });
			double num = NormalizeDEERRValue(rawValue, array[0]);
			if ("yr".Equals(array[1]))
			{
				return num;
			}
			if ("a".Equals(array[1]))
			{
				return num;
			}
			if ("ha".Equals(array[1]))
			{
				return num / 100.0;
			}
			if ("hyr".Equals(array[1]))
			{
				return num / 100.0;
			}
			throw new NotSupportedException($"Don't know how to handle year unit '{array[1]}'");
		}

		private ReferenceSystem StringToReferenceSystem(string system)
		{
			if ("eq_FK4".Equals(system, StringComparison.InvariantCultureIgnoreCase))
			{
				return ReferenceSystem.FK4;
			}
			if ("eq_FK5".Equals(system, StringComparison.InvariantCultureIgnoreCase))
			{
				return ReferenceSystem.FK5BA;
			}
			if ("ICRS".Equals(system, StringComparison.InvariantCultureIgnoreCase))
			{
				return ReferenceSystem.ICRS;
			}
			return ReferenceSystem.Other;
		}

		private double NormalizeEpoch(CatalogConfigEntry cat, string epoch, string epochUnit)
		{
			if (cat != null)
			{
				double num = 0.0;
				if (!string.IsNullOrEmpty(epoch))
				{
					if ("yr".Equals(epochUnit))
					{
						num = double.Parse(epoch, CultureInfo.InvariantCulture);
					}
					else if (".01yr".Equals(epochUnit))
					{
						num = double.Parse(epoch, CultureInfo.InvariantCulture) * 100.0;
					}
					else if ("a".Equals(epochUnit))
					{
						num = double.Parse(epoch, CultureInfo.InvariantCulture);
					}
					else if ("myr".Equals(epochUnit))
					{
						num = double.Parse(epoch, CultureInfo.InvariantCulture) * 1000.0;
					}
					else
					{
						if (!"d".Equals(epochUnit))
						{
							throw new NotSupportedException($"Don't know how to handle epoch unit '{epochUnit}'");
						}
						num = double.Parse(epoch, CultureInfo.InvariantCulture);
					}
				}
				return ((int)(cat.Flags & (CatFlags)1044480) >> 12) switch
				{
					1 => JDtoEpoch(num + 2451263.5), 
					2 => 1800.0 + num, 
					3 => 1900.0 + num, 
					4 => 1900.0 + num / 100.0, 
					5 => 2000.0, 
					6 => 1991.25, 
					7 => JDtoEpoch(num + 2451264.0), 
					8 => 1875.0, 
					9 => 1855.0, 
					10 => 1950.62, 
					11 => 1998.0, 
					12 => 1990.0 + num, 
					_ => num, 
				};
			}
			try
			{
				return double.Parse(epoch, CultureInfo.InvariantCulture);
			}
			catch (Exception)
			{
				return double.NaN;
			}
		}

		private double NormalizeEquinox(string equinox)
		{
			if (string.IsNullOrEmpty(equinox))
			{
				return double.NaN;
			}
			try
			{
				string value = equinox.Substring(0, 1);
				return double.Parse(("BJ".IndexOf(value) == -1) ? equinox : equinox.Substring(1), CultureInfo.InvariantCulture);
			}
			catch (Exception)
			{
				return double.NaN;
			}
		}

		private double NormalizeParallax(string equinox)
		{
			if (string.IsNullOrEmpty(equinox))
			{
				return double.NaN;
			}
			try
			{
				return double.Parse(equinox, CultureInfo.InvariantCulture);
			}
			catch (Exception)
			{
				return double.NaN;
			}
		}

		private int GetFieldIndex(XmlNode resourceNode, string attName, string value)
		{
			string unitName;
			string desc;
			string fieldName;
			string refName;
			return GetFieldIndex(resourceNode, attName, value, out unitName, out desc, out fieldName, out refName);
		}

		private int GetFieldIndex(XmlNode resourceNode, string attName, string value, out string unitName, out string desc)
		{
			string fieldName;
			string refName;
			return GetFieldIndex(resourceNode, attName, value, out unitName, out desc, out fieldName, out refName);
		}

		private int GetFieldIndex(XmlNode resourceNode, string attName, string value, out string unitName, out string desc, out string fieldName)
		{
			string refName;
			return GetFieldIndex(resourceNode, attName, value, out unitName, out desc, out fieldName, out refName);
		}

		private int GetFieldIndex(XmlNode resourceNode, string attName, string value, out string unitName, out string desc, out string fieldName, out string refName)
		{
			List<string> unitNames;
			List<string> descs;
			List<string> fieldNames;
			List<string> refNames;
			List<int> fieldIndexes = GetFieldIndexes(resourceNode, attName, value, out unitNames, out descs, out fieldNames, out refNames);
			if (fieldIndexes.Count > 0)
			{
				unitName = unitNames[0];
				desc = descs[0];
				fieldName = fieldNames[0];
				refName = refNames[0];
				return fieldIndexes[0];
			}
			unitName = string.Empty;
			desc = null;
			fieldName = null;
			refName = null;
			return -1;
		}

		private List<int> GetFieldIndexes(XmlNode resourceNode, string attName, string value)
		{
			List<string> unitNames;
			List<string> descs;
			List<string> fieldNames;
			List<string> refNames;
			return GetFieldIndexes(resourceNode, attName, value, out unitNames, out descs, out fieldNames, out refNames);
		}

		private List<int> GetFieldIndexes(XmlNode resourceNode, string attName, string value, out List<string> unitNames, out List<string> descs, out List<string> fieldNames, out List<string> refNames)
		{
			fieldNames = new List<string>();
			refNames = new List<string>();
			unitNames = new List<string>();
			descs = new List<string>();
			List<int> list = new List<int>();
			if (!string.IsNullOrEmpty(attName))
			{
				XmlNodeList xmlNodeList = resourceNode.SelectNodes($"./vt:FIELD[@{attName}='{value}' and @ref]", m_nsMngr);
				if (xmlNodeList == null || xmlNodeList.Count == 0)
				{
					xmlNodeList = resourceNode.SelectNodes($"./vt:FIELD[@{attName}='{value}']", m_nsMngr);
				}
				if (xmlNodeList != null)
				{
					foreach (XmlNode item in xmlNodeList)
					{
						fieldNames.Add((item.Attributes!["name"] != null) ? item.Attributes!["name"]!.Value : null);
						refNames.Add((item.Attributes!["ref"] != null) ? item.Attributes!["ref"]!.Value : null);
						unitNames.Add((item.Attributes!["unit"] != null) ? item.Attributes!["unit"]!.Value : string.Empty);
						XmlNode xmlNode2 = item.SelectSingleNode("./vt:DESCRIPTION", m_nsMngr);
						descs.Add((xmlNode2 != null) ? xmlNode2.InnerText : string.Empty);
						int xPathIndex = GetXPathIndex(item);
						list.Add(xPathIndex);
					}
					return list;
				}
			}
			return list;
		}

		private int GetXPathIndex(XmlNode node)
		{
			string name = node.Name;
			int num = -1;
			foreach (XmlNode childNode in node.ParentNode!.ChildNodes)
			{
				if (childNode.Name.Equals(name))
				{
					num++;
				}
				if (childNode.Equals(node))
				{
					return num;
				}
			}
			return num;
		}

		public static double JDtoEpoch(double JD)
		{
			return 2000.0 + (JD - 2451545.0) / 365.25;
		}
	}
}

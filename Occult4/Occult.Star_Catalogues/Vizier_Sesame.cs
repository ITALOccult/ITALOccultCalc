using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;
using System.Xml;
using Occult.SesameService;
using Occult.VizieRService;

namespace Occult.Star_Catalogues
{
	public class Vizier_Sesame
	{
		private const double Radian = 180.0 / Math.PI;

		internal static StarEquivalentList StarList;

		internal static void GetAliases(string StarName)
		{
			if (!Utilities.InternetIsAvailable())
			{
				return;
			}
			XmlDocument xmlDocument = new XmlDocument();
			try
			{
				Occult.SesameService.SesameService sesameService = new Occult.SesameService.SesameService();
				if (StarName.StartsWith("TYC", StringComparison.InvariantCultureIgnoreCase) && "1234567890".IndexOf(StarName[StarName.Length - 1]) != 0)
				{
					StarName = StarName.Substring(0, StarName.Length - 1);
				}
				string xml = sesameService.sesame(StarName, "-xI");
				xmlDocument.LoadXml(xml);
				xmlDocument.Save(Utilities.AppPath + "\\Downloaded Files\\SesameData.xml");
			}
			catch (Exception)
			{
			}
			if (xmlDocument == null)
			{
				return;
			}
			try
			{
				((Control)StarList).Show();
			}
			catch
			{
				StarList = new StarEquivalentList();
				((Control)StarList).Show();
			}
			StarList.lstStars.get_Items().Clear();
			StarList.lstStars.get_Items().Add((object)StarName);
			StarList.lstStars.get_Items().Add((object)"  =");
			foreach (XmlNode item in xmlDocument.SelectNodes("//alias")!)
			{
				StarList.lstStars.get_Items().Add((object)item.InnerText);
			}
			((Control)StarList).Focus();
		}

		internal static string GetAliaseIDs(string StarName)
		{
			string text = "";
			if (!Utilities.InternetIsAvailable())
			{
				return text;
			}
			XmlDocument xmlDocument = new XmlDocument();
			try
			{
				Occult.SesameService.SesameService sesameService = new Occult.SesameService.SesameService();
				if (StarName.StartsWith("TYC", StringComparison.InvariantCultureIgnoreCase) && "1234567890".IndexOf(StarName[StarName.Length - 1]) != 0)
				{
					StarName = StarName.Substring(0, StarName.Length - 1);
				}
				string xml = sesameService.sesame(StarName, "-xI");
				xmlDocument.LoadXml(xml);
				xmlDocument.Save(Utilities.AppPath + "\\Downloaded Files\\SesameData.xml");
			}
			catch (Exception)
			{
			}
			if (xmlDocument != null)
			{
				foreach (XmlNode item in xmlDocument.SelectNodes("//alias")!)
				{
					if (text.Length > 0)
					{
						text += ",";
					}
					text += item.InnerText;
				}
				return text;
			}
			return text;
		}

		internal static void GetXZ80Equivalents(string Catalogue, string Number)
		{
			string text = "";
			int result = 0;
			if (int.TryParse(Number, out result))
			{
				if (Catalogue == "R")
				{
					XZ80Q.Get_ZC_Star(result);
					text = "SAO " + XZ80Q.SAO;
				}
				else if (Catalogue == "S")
				{
					text = "SAO " + Number.ToString();
				}
				try
				{
					((Component)(object)StarList).Dispose();
				}
				catch
				{
				}
				if (text.Length > 2)
				{
					GetAliases(text);
				}
				_ = Catalogue == "X";
			}
		}

		public static void GetStarEquivalents(string Catalogue, string Number)
		{
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_011d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0138: Unknown result type (might be due to invalid IL or missing references)
			int result = 0;
			Number = Number.Trim();
			if (Number.Length < 1)
			{
				MessageBox.Show("Invalid number", "Invalid number");
				return;
			}
			string text = Catalogue + " " + Number;
			if (Catalogue == "TYC")
			{
				int num = Number.IndexOf("-");
				if (num < 0)
				{
					MessageBox.Show("Tycho2 number must contain '-'", "Incorrect Tycho2 format");
					return;
				}
				num = Number.IndexOf("-", num + 1);
				if (num < 0)
				{
					text += "-1";
				}
			}
			else if (Catalogue == "GSC")
			{
				if (Number.IndexOf("-") < 0)
				{
					MessageBox.Show("GSC number must contain '-'", "Incorrect GSC format");
					return;
				}
			}
			else if ((Catalogue == "BD") | (Catalogue == "CD") | (Catalogue == "CPD"))
			{
				if (Number.Trim().IndexOf(" ") < 0)
				{
					MessageBox.Show("BD/CD/CPD number must contain a space after the zone", "Incorrect BD/CD/CPD format");
					return;
				}
				if ("+-".IndexOf(Number.Substring(0, 1)) < 0)
				{
					MessageBox.Show("BD/CD/CPD number must have a '+' or '-' before the zone", "Incorrect BD/CD/CPD format");
					return;
				}
			}
			else if (!int.TryParse(Number, out result))
			{
				MessageBox.Show("Invalid number", "Invalid number");
				return;
			}
			if (Catalogue == "ZC")
			{
				XZ80Q.Get_ZC_Star(result);
				text = "SAO " + XZ80Q.SAO;
			}
			try
			{
				((Component)(object)StarList).Dispose();
			}
			catch
			{
			}
			if (text.Length > 2)
			{
				GetAliases(text);
			}
			if (!(Catalogue == "XZ"))
			{
				return;
			}
			XZ80Q.Get_XZ_Star(result);
			string text2 = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false).Trim();
			string text3 = Utilities.DEGtoDMS(XZ80Q.Dec_rad * (180.0 / Math.PI), 3, 1, MinutesOnly: false).Trim();
			if (XZ80Q.Dec_rad > 0.0)
			{
				text3 = "+" + text3;
			}
			XmlDocument xmlDocument = new XmlDocument();
			try
			{
				string xml = new Occult.VizieRService.VizieRService().cataloguesData(text2 + " " + text3, 5.0, "arcsec", "I/259");
				xmlDocument.LoadXml(xml);
			}
			catch (Exception)
			{
			}
			if (xmlDocument == null)
			{
				return;
			}
			xmlDocument.Save(Utilities.AppPath + "\\Downloaded Files\\VizierDataXZ80.xml");
			List<StarEntry> list = Votable.ParseStarEntries(File.ReadAllText(Utilities.AppPath + "\\Downloaded Files\\VizierDataXZ80.xml"));
			for (int i = 0; i < list.Count; i++)
			{
				if (list[i].StarName != null)
				{
					GetAliases(list[i].StarName);
					break;
				}
			}
		}

		internal static string GetEquivalents(double RA, double Dec)
		{
			string result = "";
			bool flag = false;
			try
			{
				((Component)(object)StarList).Dispose();
			}
			catch
			{
			}
			string text = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false).Trim();
			string text2 = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1, MinutesOnly: false).Trim();
			if (Dec > 0.0)
			{
				text2 = "+" + text2;
			}
			XmlDocument xmlDocument = new XmlDocument();
			try
			{
				string xml = new Occult.VizieRService.VizieRService().cataloguesData(text + " " + text2, 5.0, "arcsec", "I/259");
				xmlDocument.LoadXml(xml);
			}
			catch (Exception)
			{
				flag = true;
			}
			if (!flag && xmlDocument != null)
			{
				xmlDocument.Save(Utilities.AppPath + "\\Downloaded Files\\VizierDataLCeq.xml");
				List<StarEntry> list = Votable.ParseStarEntries(File.ReadAllText(Utilities.AppPath + "\\Downloaded Files\\VizierDataLCeq.xml"));
				for (int i = 0; i < list.Count; i++)
				{
					if (list[i].StarName != null)
					{
						result = GetAliaseIDs(list[i].StarName);
						break;
					}
				}
			}
			return result;
		}

		public static string GetSAOnum(double RA_ddd, double Dec_ddd, out bool ServerDown)
		{
			string result = "";
			ServerDown = false;
			new XmlDocument();
			try
			{
				string text = Utilities.DEGtoDMS(RA_ddd / 15.0, 2, 1, MinutesOnly: false).Trim();
				string text2 = Utilities.DEGtoDMS(Dec_ddd, 3, 1, MinutesOnly: false).Trim();
				if (Dec_ddd > 0.0)
				{
					text2 = "+" + text2;
				}
				string text3 = new Occult.VizieRService.VizieRService().cataloguesData(text + " " + text2, 10.0, "arcsec", "I/131A");
				int num = text3.IndexOf("<DATA><TABLEDATA>");
				if (num > 0)
				{
					int num2 = text3.IndexOf("<TR><TD>", num);
					int num3 = text3.IndexOf("</TD><TD>", num2);
					return text3.Substring(num2 + 8, num3 - num2 - 8);
				}
				return result;
			}
			catch (Exception)
			{
				ServerDown = true;
				return "-";
			}
		}
	}
}

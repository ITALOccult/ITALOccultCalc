using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Windows.Forms;

namespace Occult
{
	internal class Gaia_ESA
	{
		private const double Radian = 180.0 / Math.PI;

		internal static List<string> Stars;

		internal static List<GaiaPosition> StarList;

		internal static void GetGaiaDetailsForStarsInBox(double JD, int GaiaVersion, double RAcenter_Deg, double DecCenter_deg, double BoxSize_arcSec, double Expected_Gmag)
		{
			//IL_011b: Unknown result type (might be due to invalid IL or missing references)
			GaiaPosition.ReferenceRAdeg = RAcenter_Deg;
			GaiaPosition.ReferenceDecdeg = DecCenter_deg;
			string[] array = new string[1] { "" };
			StarList = new List<GaiaPosition>();
			Stars = new List<string>();
			string text = $"?REQUEST=doQuery&LANG=ADQL&FORMAT=csv&QUERY=SELECT+TOP+1500+*+FROM+gaiaedr{GaiaVersion}.gaia_source+WHERE+1=CONTAINS(POINT('ICRS',ra,dec),BOX('ICRS',{RAcenter_Deg:0.000000},{DecCenter_deg:0.000000},{BoxSize_arcSec / 3600.0:0.000000},{BoxSize_arcSec / 3600.0:0.000000}))";
			string requestUriString = "http://gaia.ari.uni-heidelberg.de/tap/sync" + text;
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Timeout = 10000;
				obj.Method = "GET";
				using TextReader textReader = new StreamReader(((HttpWebResponse)obj.GetResponse()).GetResponseStream());
				array = textReader.ReadToEnd().Replace("\r", "").Split(new char[1] { '\n' });
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data for Gaia star positions : \r\n\r\n" + ex.Message, "Error");
			}
			finally
			{
				GaiaPosition.SetFields("");
			}
			GaiaPosition.SetFields(array[0]);
			for (int i = 1; i < array.Length; i++)
			{
				if (array[i].Length >= 100)
				{
					GaiaPosition gaiaPosition = new GaiaPosition();
					if (gaiaPosition.DecodeStarEntry(array[i], Expected_Gmag, JD))
					{
						StarList.Add(gaiaPosition);
					}
					StarList.Sort();
				}
			}
			for (int j = 0; j < StarList.Count; j++)
			{
				Stars.Add(StarList[j].PositionOfDate_J2000(JD, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _));
			}
		}

		internal static bool GetGaiaRUWE(double JDEventDate, double RAcenter_Deg, double DecCenter_deg, double Expected_Gmag, out double RUWE, out int Issues)
		{
			//IL_0129: Unknown result type (might be due to invalid IL or missing references)
			RUWE = 0.0;
			Issues = 0;
			GaiaPosition.ReferenceRAdeg = RAcenter_Deg;
			GaiaPosition.ReferenceDecdeg = DecCenter_deg;
			string[] array = new string[1] { "" };
			StarList = new List<GaiaPosition>();
			Stars = new List<string>();
			string text = string.Format("?REQUEST=doQuery&LANG=ADQL&FORMAT=csv&QUERY=SELECT+TOP+1500+*+FROM+gaiadr2.gaia_source,gaiadr2.ruwe+WHERE+1=CONTAINS(POINT('ICRS',ra,dec),BOX('ICRS',{1:0.000000},{2:0.000000},{3:0.000000},{4:0.000000}))+AND+gaia_source.source_id=ruwe.source_id", 2, RAcenter_Deg, DecCenter_deg, 0.005583333333333333, 0.005583333333333333);
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			string requestUriString = "http://gaia.ari.uni-heidelberg.de/tap/sync" + text;
			try
			{
				HttpWebRequest obj = (HttpWebRequest)WebRequest.Create(requestUriString);
				obj.Timeout = 10000;
				obj.Method = "GET";
				using TextReader textReader = new StreamReader(((HttpWebResponse)obj.GetResponse()).GetResponseStream());
				array = textReader.ReadToEnd().Replace("\r", "").Split(new char[1] { '\n' });
			}
			catch (Exception ex)
			{
				MessageBox.Show("Error requesting data for Gaia position \r\n\r\n" + ex.Message, "Error");
			}
			GaiaPosition.SetFields(array[0]);
			for (int i = 1; i < array.Length; i++)
			{
				if (array[i].Length >= 100)
				{
					GaiaPosition gaiaPosition = new GaiaPosition();
					if (gaiaPosition.DecodeStarEntry(array[i], Expected_Gmag, JDEventDate))
					{
						StarList.Add(gaiaPosition);
					}
					StarList.Sort();
				}
			}
			for (int j = 0; j < StarList.Count; j++)
			{
				Stars.Add(StarList[j].PositionOfDate_J2000(JDEventDate, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _));
			}
			double num = 1.0;
			if (JDEventDate < 2451545.0)
			{
				num = 2.0;
			}
			if (JDEventDate < 2444239.0)
			{
				num = 4.0;
			}
			if (JDEventDate < 2433282.0)
			{
				num = 15.0;
			}
			int num2 = -1;
			if (Stars.Count > 1)
			{
				if (StarList[0].OffsetFromCenter < num)
				{
					num2 = 0;
				}
				if ((StarList[1].OffsetFromCenter < num) & (StarList[1].Mg < StarList[0].Mg))
				{
					num2 = 1;
				}
			}
			else if (Stars.Count > 0 && StarList[0].OffsetFromCenter < num)
			{
				num2 = 0;
			}
			if (num2 >= 0)
			{
				RUWE = StarList[num2].RUWEvalue;
				Issues = StarList[num2].Issues;
				return true;
			}
			return false;
		}
	}
}

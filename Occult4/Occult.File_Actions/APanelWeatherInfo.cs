using System;
using System.Globalization;
using System.IO;
using System.Net;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.File_Actions
{
	public class APanelWeatherInfo
	{
		internal readonly double Longitude;

		internal readonly double Latitude;

		internal readonly DateTime InitialTime;

		internal static DateTime TimeInitial;

		internal readonly DateTime EndTime;

		internal static readonly int[] CloudCover = new int[24];

		internal static readonly int[] LiftedIndex = new int[24];

		internal static readonly int[] TempC = new int[24];

		internal static CloudforSite SiteCloud;

		public static CloudMap Cloud_Map;

		public APanelWeatherInfo(string rawHtml)
		{
			try
			{
				string[] array = rawHtml.Split(new string[1] { "<br>" }, StringSplitOptions.RemoveEmptyEntries);
				string[] array2 = array[0].Split(new char[1] { '\t' });
				Longitude = double.Parse(array2[1], CultureInfo.InvariantCulture);
				Latitude = double.Parse(array2[2], CultureInfo.InvariantCulture);
				string text = array2[4];
				InitialTime = new DateTime(int.Parse(text.Substring(0, 4)), int.Parse(text.Substring(4, 2)), int.Parse(text.Substring(6, 2)), int.Parse(text.Substring(8, 2)), 0, 0);
				TimeInitial = InitialTime;
				array2 = array[2].Split(new char[1] { '\t' });
				for (int i = 0; i < Math.Min(24, array2.Length); i++)
				{
					InitialTime.AddHours(3 * (i + 1));
					int num = int.Parse(array2[i].Trim());
					CloudCover[i] = num;
				}
				array2 = array[5].Split(new char[1] { '\t' });
				for (int j = 0; j < Math.Min(24, array2.Length); j++)
				{
					int num2 = int.Parse(array2[j].Trim());
					LiftedIndex[j] = num2;
				}
				array2 = array[7].Split(new char[1] { '\t' });
				for (int k = 0; k < Math.Min(24, array2.Length); k++)
				{
					int num3 = int.Parse(array2[k].Trim());
					TempC[k] = num3;
				}
				EndTime = InitialTime.AddHours(3 * array2.Length);
			}
			catch
			{
			}
		}

		public static int GetCloudCoverForTime(DateTime ut)
		{
			TimeSpan timeSpan = new TimeSpan(ut.Ticks - TimeInitial.Ticks);
			if (timeSpan.TotalHours > 0.0 && timeSpan.TotalHours < 72.0)
			{
				int num = Math.Max(0, Math.Min(24, (int)timeSpan.TotalHours / 3));
				return CloudCover[num];
			}
			return -1;
		}

		public int GetLiftedIndexForTime(DateTime ut)
		{
			long ticks = ut.Ticks;
			DateTime initialTime = InitialTime;
			TimeSpan timeSpan = new TimeSpan(ticks - initialTime.Ticks);
			if (timeSpan.TotalHours > 0.0 && timeSpan.TotalHours < 72.0)
			{
				int num = Math.Max(0, Math.Min(24, (int)timeSpan.TotalHours / 3));
				return LiftedIndex[num];
			}
			return -1;
		}

		public static int GetTcForTime(DateTime ut)
		{
			TimeSpan timeSpan = new TimeSpan(ut.Ticks - TimeInitial.Ticks);
			if (timeSpan.TotalHours > 0.0 && timeSpan.TotalHours < 72.0)
			{
				int num = Math.Max(0, Math.Min(24, (int)timeSpan.TotalHours / 3));
				return TempC[num];
			}
			return -1;
		}

		internal static string Get7TimerWeatherForLocation(double lng, double lat)
		{
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create(string.Format(Settings.Default.SevenTimer + "/V3/exe/apanel_pure.php?lon={0}&lat={1}&en", lng.ToString(CultureInfo.InvariantCulture), lat.ToString(CultureInfo.InvariantCulture)));
			httpWebRequest.UserAgent = "Occult v4";
			httpWebRequest.Timeout = 120000;
			try
			{
				using Stream stream = httpWebRequest.GetResponse().GetResponseStream();
				using TextReader textReader = new StreamReader(stream);
				return textReader.ReadToEnd();
			}
			catch
			{
				MessageBox.Show("Error getting data from 7Timer Server", "7Timer Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return "";
			}
		}

		public static void ShowWeatherForHome()
		{
			double longitude = double.Parse(Settings.Default.Site_Longitude_dd_d);
			double latitude = double.Parse(Settings.Default.Site_Latitude_dd_d);
			ShowSiteCloud(longitude, latitude);
		}

		internal static void ShowSiteCloud(double Longitude, double Latitude)
		{
			try
			{
				((Control)SiteCloud).Show();
			}
			catch
			{
				SiteCloud = new CloudforSite();
				((Control)SiteCloud).Show();
			}
			((Control)SiteCloud).set_Cursor(Cursors.get_WaitCursor());
			new APanelWeatherInfo(Get7TimerWeatherForLocation(Longitude, Latitude));
			((Control)SiteCloud.label1).set_Text(string.Format("for Longitude {0,3:F2}", Longitude) + string.Format(",  Latitude {0,3:F2}", Latitude));
			SiteCloud.lstCloud.get_Items().Clear();
			SiteCloud.lstCloud.get_Items().Add((object)"    (time is UT)");
			SiteCloud.lstCloud.get_Items().Add((object)"   y   m  d   h  cloud    Tc");
			for (int i = 0; i < 24; i++)
			{
				TimeInitial = TimeInitial.AddHours(3.0);
				StringBuilder stringBuilder = new StringBuilder();
				if ((i == 0) | (TimeInitial.Hour == 0))
				{
					stringBuilder.Append(TimeInitial.Year.ToString());
					stringBuilder.Append(" " + Utilities.ShortMonths[TimeInitial.Month]);
					stringBuilder.Append(TimeInitial.Day.ToString().PadLeft(3));
				}
				else
				{
					stringBuilder.Append("".PadRight(11));
				}
				stringBuilder.Append(TimeInitial.Hour.ToString().PadLeft(4));
				stringBuilder.AppendFormat("  {0,4:F0}%", 12.5 * (double)CloudCover[i]);
				stringBuilder.AppendFormat("  {0,4:F0}", TempC[i]);
				SiteCloud.lstCloud.get_Items().Add((object)stringBuilder.ToString());
			}
			((Control)SiteCloud).set_Cursor(Cursors.get_Default());
			((Control)SiteCloud).Focus();
		}

		public static void ShowCloudMap()
		{
			try
			{
				((Control)Cloud_Map).Show();
			}
			catch
			{
				Cloud_Map = new CloudMap();
				((Control)Cloud_Map).Show();
			}
			((Control)Cloud_Map).Focus();
		}
	}
}

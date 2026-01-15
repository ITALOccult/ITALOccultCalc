using System.IO;

namespace Occult.Lunar_Observations
{
	public class RGOsites
	{
		private static FileStream RGOfile;

		private static BinaryReader RGO;

		internal static void OpenRGOSites()
		{
			RGOfile = new FileStream(Utilities.AppPath + "\\Resource Files\\Archive RGO Sites.dat", FileMode.Open, FileAccess.Read);
			RGO = new BinaryReader(RGOfile);
		}

		internal static void CloseRGOSites()
		{
			RGO.Close();
		}

		internal static bool RGOSite(string Site, out string SiteString, out string SiteNameFromFile, out string Observer)
		{
			string text = "";
			bool flag = false;
			string text2 = Site;
			if (text2.Substring(0, 1) == " ")
			{
				text2 = text2.Remove(0, 1).Insert(0, "0");
			}
			if (text2.Substring(1, 1) == " ")
			{
				text2 = text2.Remove(1, 1).Insert(1, "0");
			}
			if (text2.Substring(4, 1) == " ")
			{
				text2 = text2.Remove(4, 1).Insert(4, "0");
			}
			int num = 0;
			int num2 = (int)(RGOfile.Length / 94 - 1);
			flag = false;
			do
			{
				int num3 = (num2 + num) / 2;
				RGOfile.Seek(94 * num3, SeekOrigin.Begin);
				string text3 = new string(RGO.ReadChars(92));
				if (text3.Substring(0, 6) == text2)
				{
					flag = true;
					text = text3;
					break;
				}
				if (text3.Substring(0, 6).CompareTo(text2) > 0)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			string text4;
			if (flag)
			{
				text4 = "TA                  " + text.Substring(7, 4) + text.Substring(12, 2) + text.Substring(15, 5) + " " + text.Substring(20, 3) + text.Substring(24, 2) + text.Substring(27, 5) + " " + text.Substring(37, 2) + " " + text.Substring(33, 4) + ".0M";
				string text3 = text.PadRight(30).Substring(40, 30);
				SiteNameFromFile = "OA  " + text3.Substring(0, 28) + text.Substring(0, 6);
				Observer = "OA*                                          ";
				if (text.Substring(71, 21).Trim().Length > 0)
				{
					Observer = "OA  " + text.Substring(71, 21);
				}
			}
			else
			{
				text4 = "TA  NAM  10.0  100.0   00 00 00    E  00 00 00    N    0       ";
				SiteNameFromFile = (Observer = "OA*                                          ");
			}
			SiteString = text4;
			return flag;
		}
	}
}

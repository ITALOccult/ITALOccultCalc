using System.IO;

namespace Occult.Lunar_Observations
{
	public static class ILOCsites
	{
		private static FileStream TELFfile;

		private static BinaryReader TELF;

		private static FileStream OBSFfile;

		private static BinaryReader OBSF;

		internal static void OpenILOCSites()
		{
			TELFfile = new FileStream(Utilities.AppPath + "\\Resource Files\\Archive ILOC sites.dat", FileMode.Open, FileAccess.Read);
			TELF = new BinaryReader(TELFfile);
		}

		internal static void CloseILOCSites()
		{
			TELF.Close();
		}

		internal static bool ILOCSite(string Site, out string SiteString, out string SiteNameFromFile)
		{
			string text = "";
			bool flag = false;
			int num = 0;
			int num2 = (int)(TELFfile.Length / 103 - 1);
			string text2 = "";
			do
			{
				int num3 = (num2 + num) / 2;
				TELFfile.Seek(103 * num3, SeekOrigin.Begin);
				string text3 = new string(TELF.ReadChars(103));
				if (text3.Substring(0, 9) == Site)
				{
					flag = true;
					text = text3;
					break;
				}
				if (text3.Substring(0, 9).CompareTo(Site) > 0)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			if (flag)
			{
				text2 = "TA  " + text.Substring(90, 3) + " " + text.Substring(93, 4) + "  " + text.Substring(97, 4) + "  " + text.Substring(10, 11) + " " + text.Substring(21, 10) + " " + text.Substring(31, 2) + " " + text.Substring(33, 7);
				SiteNameFromFile = "OA  " + text.Substring(40, 20) + "".PadRight(8) + text.Substring(0, 5);
			}
			else
			{
				text2 = "TA  NAM  10.0  100.0   00 00 00    E  00 00 00    N    0       ";
				SiteNameFromFile = "OA*                                          ";
			}
			SiteString = text2;
			return flag;
		}

		internal static void OpenILOCObserver()
		{
			OBSFfile = new FileStream(Utilities.AppPath + "\\Resource Files\\Archive ILOC Observers.dat", FileMode.Open, FileAccess.Read);
			OBSF = new BinaryReader(OBSFfile);
		}

		internal static void CloseILOCObserver()
		{
			OBSF.Close();
		}

		internal static string ILOCObserver(string Site)
		{
			int num = 0;
			int num2 = (int)(OBSFfile.Length / 37 - 1);
			string result = "OA*                                          ";
			do
			{
				int num3 = (num2 + num) / 2;
				OBSFfile.Seek(37 * num3, SeekOrigin.Begin);
				string text = new string(OBSF.ReadChars(37));
				if (text.Substring(0, 9) == Site)
				{
					result = "OA  " + text.Substring(10, 25);
					break;
				}
				if (text.Substring(0, 9).CompareTo(Site) > 0)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num <= num2);
			return result;
		}
	}
}

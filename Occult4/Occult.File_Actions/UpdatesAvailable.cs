using System;
using System.IO;
using System.Net;
using Occult.Properties;

namespace Occult.File_Actions
{
	public static class UpdatesAvailable
	{
		public static DateTime AstDyS2Date = DateTime.Now;

		public static bool DataUpdatesAvailable()
		{
			UpdateAvailable(out var Aster, out var Lunar, out var Address, out var dT, out var BinaryAst, out var AsterRings, out var AsterDia, out var _, out var _, out var _, out var LunarArchive, out var XZcat, out var _, out var Tycho, out var DE, out var Lola, out var StarDias, out var CameraDelays, out var Kepler, out var Gaia, out var _, out var Gaia3, out var _, out var AstDyS, out var _);
			return ((Aster || Lunar || Address || dT || BinaryAst || AsterRings || AsterDia || LunarArchive || XZcat || Tycho || DE || Lola) | DownloadFlagsForDoublesVariables()) || StarDias || CameraDelays || Kepler || Gaia || Gaia3 || AstDyS;
		}

		internal static void UpdateAvailable(out bool Aster, out bool Lunar, out bool Address, out bool dT, out bool BinaryAst, out bool AsterRings, out bool AsterDia, out bool AsterClass, out bool ISAM, out bool Shapes, out bool LunarArchive, out bool XZcat, out bool LightCurve, out bool Tycho, out bool DE, out bool Lola128, out bool StarDias, out bool CameraDelays, out bool Kepler2, out bool Gaia9, out bool Gaia12, out bool Gaia14, out bool Gaia16, out bool AstDyS, out bool AAVSO_Occult)
		{
			string date = "";
			string date2 = "";
			string date3 = "";
			string date4 = "";
			string date5 = "";
			string date6 = "";
			string date7 = "";
			string date8 = "";
			string date9 = "";
			string date10 = "";
			string date11 = "";
			string date12 = "";
			string date13 = "";
			string date14 = "";
			string date15 = "";
			string date16 = "";
			string date17 = "";
			string date18 = "";
			string date19 = "";
			string date20 = "";
			string date21 = "";
			Aster = (Lunar = (Address = (dT = (BinaryAst = (AsterRings = (AsterDia = (AsterClass = (ISAM = (Shapes = (LunarArchive = (XZcat = (LightCurve = (Tycho = (DE = (Lola128 = (StarDias = (CameraDelays = (Kepler2 = (Gaia9 = (Gaia12 = (Gaia14 = (Gaia16 = (AstDyS = (AAVSO_Occult = false))))))))))))))))))))))));
			DateTime date22 = File.GetLastWriteTime(Downloads.AsteroidObsFile).ToUniversalTime().Date;
			DateTime date23 = File.GetLastWriteTime(Downloads.LunarRecentFile).ToUniversalTime().Date;
			DateTime date24 = File.GetLastWriteTime(Downloads.ReportingAddressFile).ToUniversalTime().Date;
			DateTime date25 = File.GetLastWriteTime(Downloads.DeltaTAFile).ToUniversalTime().Date;
			DateTime date26 = File.GetLastWriteTime(Downloads.BinaryAsteroidFile).ToUniversalTime().Date;
			DateTime date27 = File.GetLastWriteTime(Downloads.AsteroidDiametersFile).ToUniversalTime().Date;
			_ = File.GetLastWriteTime(Downloads.ShapeFile).ToUniversalTime().Date;
			DateTime date28 = File.GetLastWriteTime(Downloads.XZfile).ToUniversalTime().Date;
			DateTime date29 = File.GetLastWriteTime(Downloads.Tycho2File).ToUniversalTime().Date;
			DateTime date30 = File.GetLastWriteTime(Downloads.JPL_DEfile).ToUniversalTime().Date;
			DateTime date31 = File.GetLastWriteTime(Downloads.LunarOldFiles).ToUniversalTime().Date;
			DateTime date32 = File.GetLastWriteTime(Downloads.Lola128File).ToUniversalTime().Date;
			DateTime date33 = File.GetLastWriteTime(Downloads.StarDiaFile).ToUniversalTime().Date;
			DateTime date34 = File.GetLastWriteTime(Downloads.Kepler2File).ToUniversalTime().Date;
			_ = File.GetLastWriteTime(Downloads.AAVSOindexFile).ToUniversalTime().Date;
			DateTime dateTime;
			try
			{
				dateTime = File.GetLastWriteTime(Downloads.CameraDelayFile).Date;
			}
			catch
			{
				dateTime = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime2;
			try
			{
				dateTime2 = File.GetLastWriteTime(Downloads.AsteroidRingFile).Date;
			}
			catch
			{
				dateTime2 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime3;
			try
			{
				dateTime3 = File.GetLastWriteTime(Downloads.AsteroidClassFile).Date;
			}
			catch
			{
				dateTime3 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime4;
			try
			{
				dateTime4 = File.GetLastWriteTime(Downloads.LightCurveFile).Date;
			}
			catch
			{
				dateTime4 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime5;
			try
			{
				dateTime5 = File.GetLastWriteTime(Downloads.ISAMFile).Date;
			}
			catch
			{
				dateTime5 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime6;
			try
			{
				dateTime6 = File.GetLastWriteTime(Downloads.ShapeFile).Date;
			}
			catch
			{
				dateTime6 = DateTime.Today.AddYears(-2);
			}
			try
			{
				_ = File.GetLastWriteTime(Downloads.Gaia9File).Date;
			}
			catch
			{
				DateTime.Today.AddYears(-2);
			}
			DateTime dateTime7;
			try
			{
				dateTime7 = File.GetLastWriteTime(Downloads.Gaia12File).Date;
			}
			catch
			{
				dateTime7 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime8;
			try
			{
				dateTime8 = File.GetLastWriteTime(Downloads.Gaia14File).Date;
			}
			catch
			{
				dateTime8 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime9;
			try
			{
				dateTime9 = File.GetLastWriteTime(Downloads.Gaia16File).Date;
			}
			catch
			{
				dateTime9 = DateTime.Today.AddYears(-2);
			}
			DateTime dateTime10;
			try
			{
				dateTime10 = File.GetLastWriteTime(Downloads.AstDysFile).Date;
			}
			catch
			{
				dateTime10 = DateTime.Today.AddYears(-2);
			}
			if (!File.Exists(Downloads.DownloadContolFile))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(Downloads.DownloadContolFile))
			{
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					try
					{
						if (text.Substring(1, 8) == "Asteroid")
						{
							date = text.Substring(10);
						}
						else if (text.Substring(1, 5) == "Lunar")
						{
							date2 = text.Substring(7);
						}
						else if (text.Substring(1, 9) == "Addresses")
						{
							date3 = text.Substring(11);
						}
						else if (text.Substring(1, 6) == "deltaT")
						{
							date4 = text.Substring(8);
						}
						else if (text.Substring(1, 6) == "Binary")
						{
							date5 = text.Substring(8);
						}
						else if (text.Substring(1, 5) == "Rings")
						{
							date6 = text.Substring(7);
						}
						else if (text.Substring(1, 9) == "Diameters")
						{
							date7 = text.Substring(11);
						}
						else if (text.Substring(1, 7) == "Classes")
						{
							date8 = text.Substring(9);
						}
						else if (text.Substring(1, 7) == "Archive")
						{
							date11 = text.Substring(9);
						}
						else if (text.Substring(1, 2) == "XZ")
						{
							date12 = text.Substring(4);
						}
						else if (text.Substring(1, 6) == "Tycho2")
						{
							date14 = text.Substring(8);
						}
						else if (text.Substring(1, 5) == "JPLDE")
						{
							date15 = text.Substring(7);
						}
						else if (text.Substring(1, 7) == "Lola128")
						{
							date16 = text.Substring(9);
						}
						else if (text.Substring(1, 7) == "StarDia")
						{
							date17 = text.Substring(9);
						}
						else if (text.Substring(1, 12) == "CameraDelays")
						{
							text.Substring(14);
						}
						else if (text.Substring(1, 7) == "Kepler2")
						{
							date18 = text.Substring(9);
						}
						else if (text.Substring(1, 6) == "LCurve")
						{
							date13 = text.Substring(8);
						}
						else if (text.Substring(1, 4) == "ISAM")
						{
							date9 = text.Substring(6);
						}
						else if (text.Substring(1, 6) == "Shapes")
						{
							date10 = text.Substring(8);
						}
						else if (text.Substring(1, 5) == "Gaia9")
						{
							text.Substring(8);
						}
						else if (text.Substring(1, 6) == "Gaia12")
						{
							date19 = text.Substring(8);
						}
						else if (text.Substring(1, 6) == "Gaia14")
						{
							date20 = text.Substring(8);
						}
						else if (text.Substring(1, 6) == "Gaia16")
						{
							date21 = text.Substring(8);
						}
						else if (text.Substring(1, 5) == "AAVSO")
						{
							text.Substring(7);
						}
					}
					catch
					{
					}
				}
			}
			if (Utilities.InternetIsAvailable())
			{
				try
				{
					Dns.GetHostEntry("https://newton.spacedys.com/astdys2");
					string text2 = http.Download_WebPage("https://newton.spacedys.com/astdys/update_done");
					if (text2.Length > 1)
					{
						if (!int.TryParse(text2.Substring(8, 2), out var result))
						{
							result = 1;
						}
						int i;
						for (i = 1; i < 13 && !(Utilities.ShortMonths[i] == text2.Substring(4, 3)); i++)
						{
						}
						if (!int.TryParse(text2.Substring(24, 4), out var result2))
						{
							result2 = 1600;
						}
						AstDyS2Date = new DateTime(result2, i, result);
					}
					else
					{
						AstDyS2Date = new DateTime(2000, 1, 1);
					}
				}
				catch
				{
					AstDyS2Date = new DateTime(2000, 1, 1);
				}
			}
			Aster = ControlDate(date) > date22;
			Lunar = ControlDate(date2) > date23;
			Address = ControlDate(date3) > date24;
			dT = ControlDate(date4) > date25;
			BinaryAst = ControlDate(date5) > date26;
			AsterRings = ControlDate(date6) > dateTime2;
			AsterDia = ControlDate(date7) > date27;
			AsterClass = ControlDate(date8) > dateTime3;
			ISAM = ControlDate(date9) > dateTime5;
			Shapes = ControlDate(date10) > dateTime6;
			LunarArchive = ControlDate(date11) > date31;
			XZcat = ControlDate(date12) > date28;
			LightCurve = ControlDate(date13) > dateTime4;
			Tycho = ControlDate(date14) > date29;
			DE = ControlDate(date15) > date30;
			Lola128 = ControlDate(date16) > date32;
			StarDias = ControlDate(date17) > date33;
			CameraDelays = ControlDate(date17) > dateTime;
			Kepler2 = ControlDate(date18) > date34;
			Gaia12 = ControlDate(date19) > dateTime7;
			Gaia14 = ControlDate(date20) > dateTime8;
			Gaia16 = ControlDate(date21) > dateTime9;
			AstDyS = AstDyS2Date > dateTime10.AddDays(2.0);
		}

		private static DateTime ControlDate(string Date)
		{
			Date = Date.PadRight(8);
			if (!int.TryParse(Date.Substring(0, 2), out var result))
			{
				result = 1;
			}
			if (!int.TryParse(Date.Substring(2, 2), out var result2))
			{
				result2 = 1;
			}
			if (!int.TryParse(Date.Substring(4, 4), out var result3))
			{
				result3 = 1600;
			}
			return new DateTime(result3, result2, result).Date;
		}

		private static bool DownloadFlagsForDoublesVariables()
		{
			bool flag = false;
			int months = -(new int[5] { 1, 3, 6, 12, 60 })[Settings.Default.DownloadFileAge];
			DateTime value = DateTime.Now.AddMonths(months);
			if (File.Exists(Downloads.WDSfile))
			{
				flag = File.GetLastWriteTime(Downloads.WDSfile).CompareTo(value) < 0;
			}
			else
			{
				flag = true;
			}
			if (File.Exists(Downloads.WDScodesFile))
			{
				flag = File.GetLastWriteTime(Downloads.WDScodesFile).CompareTo(value) < 0;
			}
			else
			{
				flag = true;
			}
			if (File.Exists(Downloads.Int4File))
			{
				flag = File.GetLastWriteTime(Downloads.Int4File).CompareTo(value) < 0;
			}
			else
			{
				flag = true;
			}
			if (File.Exists(Downloads.AAVSOFullFile))
			{
				flag = File.GetLastWriteTime(Downloads.AAVSOFullFile).CompareTo(value) < 0;
			}
			else
			{
				flag = true;
			}
			if (File.Exists(Downloads.CALLfile))
			{
				flag = File.GetLastWriteTime(Downloads.CALLfile).CompareTo(value) < 0;
			}
			else
			{
				flag = true;
			}
			if (File.Exists(Downloads.DoubleOrbitFile))
			{
				return File.GetLastWriteTime(Downloads.DoubleOrbitFile).CompareTo(value) < 0;
			}
			return true;
		}
	}
}

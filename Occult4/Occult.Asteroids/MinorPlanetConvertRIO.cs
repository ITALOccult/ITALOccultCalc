using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using Microsoft.VisualBasic;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Asteroids
{
	public class MinorPlanetConvertRIO
	{
		private const double Radian = 180.0 / Math.PI;

		private static List<RIO_Updates> RUpdate = new List<RIO_Updates>();

		private static int pRAstar;

		private static int pDecstar;

		private static int pRAObj;

		private static int pDecObj;

		private static int pClosestSepn;

		private static int pVel;

		private static int pPAvel;

		private static int pDelta;

		private static int pRmag;

		private static int pErrRA;

		private static int pErrDec;

		private static int pRUWE;

		private static int pDuplicate;

		private static string raSTAR;

		private static string decSTAR;

		private static string raOBJECT;

		private static string decOBJECT;

		private static double RAStar;

		private static double DecStar;

		private static double RAObject;

		private static double DecObject;

		private static double Velocity;

		private static double ClosestSepn;

		private static double PAofConjunction;

		private static double Delta;

		private static double Mv;

		private static double ErrRA;

		private static double ErrDec;

		private static double Sec;

		private static double Arg2;

		private static double RUWE;

		private static int Year;

		private static int Month;

		private static int Day;

		private static int Hr;

		private static int Min;

		private static int Arg3;

		private static int Duplicate;

		private static int IDNumber = 0;

		private static int UpdateCounter = -1;

		private static string IDName = "";

		private static string IDNum = "";

		private static string IDforUpdateMatch = "";

		private static double MAnom = 0.0;

		private static double Perih = 0.0;

		private static double node = 0.0;

		private static double i = 0.0;

		private static double e = 0.0;

		private static double A = 0.0;

		private static double Q = 0.0;

		private static double Mag_H0 = 0.0;

		private static double Mag_CoeffLogR = 5.0;

		private static double MagG = 0.0;

		private static string Epoch = "20000101.00000";

		private static double AsteroidDiameter = 200.0;

		private static double MJDofPrediction = 0.0;

		private static bool InvolvesMajorPlanet = false;

		private static bool InvolvesComet = false;

		private static bool InvolvesAsteroidMoon = false;

		private static string PlanetID = "0";

		private static string IDmoonID = "0";

		private static int NumOfRings = 0;

		private static int NumOfMoons = 0;

		private static string AsteroidClass = "";

		private static bool UCAC4Present = false;

		private static bool PPMXLpresent = false;

		private static bool NOMADpresent = false;

		private static bool SuppressNonMatchedStars = false;

		private static bool ExcludeOldEvents = false;

		private static double JD_OneMonthAgo = 0.0;

		private static int EventCounter = 0;

		internal static bool ShowRioMessage(bool Reminder)
		{
			//IL_000a: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Invalid comparison between Unknown and I4
			string text = "";
			MessageBoxButtons val;
			if (Reminder)
			{
				val = (MessageBoxButtons)0;
				text += "REMINDER\r\n\r\n";
			}
			else
			{
				val = (MessageBoxButtons)4;
			}
			text += "By using this RIO-TNO prediction you understand that:\r\n\r\n* This was made available to you thanks to cooperation\r\n  between Occult, OccultWatcher and the Rio group\r\n on TNO occultations.\r\n\r\n* The Rio group is composed of researchers from Brazil who\r\n  act in ALL stages of the study of small bodies of the solar\r\n  system through stellar occultations.\r\n\r\n* Years of man power and financial resources are applied to\r\n  provide precise prediction of stellar occultations by\r\n  Centaurs and TNOs.\r\n\r\n* Financial support for the Rio group depends upon the\r\n  publication of successful observations.\r\n\r\n* Without harming the efforts/credits of any one, the Rio\r\n  group wants to collaborate with the analysis and possible\r\n  publications of any detected event from the RIO-TNO feed.\r\n\r\n* Taking the above into account, whenever a RIO-TNO event is\r\n  detected, contact ribas(at)on.br to continue this\r\n  collaborative work.\r\n\r\n\r\n";
			if (!Reminder)
			{
				text += "Processing will only continue if you agree to collaborate with\r\nthe RIO group with any observations.\r\n\r\nDO YOU AGREE TO COLLABORATE WITH THE RIO-TNO GROUP\r\nWITH ANY OBSERVATIONS MADE OF EVENTS IN THIS FEED?";
			}
			if ((int)MessageBox.Show(text, "Right to use the RIO - TNO predictions", val, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			return true;
		}

		public static void DownloadANDconvertRIO(bool ExcludeOld, bool UseExistingDownloads, bool UseRIO_UpdateFilesOnly)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Invalid comparison between Unknown and I4
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012b: Invalid comparison between Unknown and I4
			//IL_0191: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Invalid comparison between Unknown and I4
			//IL_019a: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a1: Expected O, but got Unknown
			//IL_01f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f7: Invalid comparison between Unknown and I4
			//IL_0205: Unknown result type (might be due to invalid IL or missing references)
			//IL_020c: Expected O, but got Unknown
			//IL_025c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Invalid comparison between Unknown and I4
			//IL_03ca: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			if ((int)MessageBox.Show("There are two sources of RIO predictions:\r\n\r\n1. TNOs and Centaurs\r\n\r\n2. Satellites of Jupiter\r\n\r\nClick 'Yes' to convert TNOs and Cenaurs\r\nClick 'No' to convert Satellites of Jupiter", "Select data source", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
			{
				flag = true;
			}
			string text = "TNOs_RIO";
			if (flag)
			{
				text = "JupSats";
			}
			DateTime dateTime = DateTime.Now.ToUniversalTime();
			string text2 = "_" + dateTime.Year + dateTime.Month.ToString().PadLeft(2, '0') + dateTime.Day.ToString().PadLeft(2, '0');
			string text3 = Utilities.AppPath + "\\Generated Files\\" + text + text2 + ".xml";
			string MessageText = "No stars will be matched to a catalogue";
			string AvailableCatalogues = "";
			string text4 = Utilities.AppPath + "\\downloaded Files\\RIO_Main.dat";
			string text5 = Utilities.AppPath + "\\downloaded Files\\RIO_Updates.dat";
			string text6 = Utilities.AppPath + "\\DownLoaded Files\\RIO_Update_Dates.dat";
			bool flag2 = true;
			if (!ShowRioMessage(Reminder: false))
			{
				return;
			}
			GetAvailableCatalogues(ref MessageText, ref AvailableCatalogues);
			SuppressNonMatchedStars = false;
			if ((int)MessageBox.Show(MessageText, "Suppress unmatched stars", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				SuppressNonMatchedStars = true;
			}
			ExcludeOldEvents = ExcludeOld;
			DateTime dateTime2 = DateTime.Now.AddMonths(-1);
			JD_OneMonthAgo = Utilities.JD_from_Date(dateTime2.Year, dateTime2.Month, dateTime2.Day);
			if (!UseExistingDownloads && !UseRIO_UpdateFilesOnly)
			{
				flag2 = Download_RIO_SourceFiles(flag);
			}
			if (UseRIO_UpdateFilesOnly)
			{
				if ((int)MessageBox.Show("You need to select the 'table_occult.txt' and 'LOG.dat' files to use.\r\n\r\nTypically, these files will have been received by Email.\r\n\r\nYou should save those files in a location of your choice. You can\r\nrename them by adding (for example) a date or reference number to\r\nthe file name - but leave the basic name and the file extension\r\nunchanged.\r\n\r\nSelect each file individually in the next 2 dialogs.", "Locate the RIO 'table_occult.txt' and 'LOG.dat' files", (MessageBoxButtons)1, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 2)
				{
					return;
				}
				OpenFileDialog val = new OpenFileDialog();
				((FileDialog)val).set_DefaultExt("txt");
				((FileDialog)val).set_FileName("table_occult");
				((FileDialog)val).set_Title("Select 'table_occult.txt file to use");
				((FileDialog)val).set_SupportMultiDottedExtensions(true);
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Downloaded files");
				((FileDialog)val).set_Filter("txt files (*.txt)|*.txt|All files (*.*)|*.*");
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				text5 = ((FileDialog)val).get_FileName();
				val = new OpenFileDialog();
				((FileDialog)val).set_DefaultExt("dat");
				((FileDialog)val).set_FileName("LOG");
				((FileDialog)val).set_Title("Select 'LOG.dat file to use");
				((FileDialog)val).set_SupportMultiDottedExtensions(true);
				((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Downloaded files");
				((FileDialog)val).set_Filter("dat files (*.dat)|*.dat|All files (*.*)|*.*");
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				text6 = ((FileDialog)val).get_FileName();
			}
			if ((File.Exists(text5) & File.Exists(text6) & (UseRIO_UpdateFilesOnly = true | File.Exists(text4))) || !flag2)
			{
				CreateUpdateList(text6);
				if (MinorPlanetOccultationElements.BinaryElements.Count < 1)
				{
					MinorPlanetOccultationElements.CreateBinaryElementList();
				}
				if (NOMADpresent)
				{
					NOMAD.InitialiseNOMAD();
				}
				if (PPMXLpresent)
				{
					PPMXL.InitialisePPMXL();
				}
				OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
				DisplayMPOccultations.OccElements.Clear();
				Read_LuckyStar_RIO_File(text5, IsUpdateFile: true, 0, "RIO_Update:");
				if (!UseRIO_UpdateFilesOnly)
				{
					Read_LuckyStar_RIO_File(text4, IsUpdateFile: false, 0, "RIO_Main:");
				}
				if (NOMADpresent)
				{
					NOMAD.ReleaseNOMAD();
				}
				OccultationElements.SortField = 0;
				DisplayMPOccultations.OccElements.Sort();
				using (StreamWriter streamWriter = new StreamWriter(text3))
				{
					streamWriter.WriteLine("<Occultations>");
					for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
					{
						streamWriter.Write(DisplayMPOccultations.OccElements[i].XML_Elements());
					}
					streamWriter.WriteLine("</Occultations>");
				}
				DisplayMPOccultations.ShowListAndDisplay();
				DisplayMPOccultations.ListAndDisplay.SetSite();
				DisplayMPOccultations.ListAndDisplay.SetSiteTests();
				DisplayMPOccultations.ListAndDisplay.SortAndDisplay(0);
				DisplayMPOccultations.ListAndDisplay.SetDisplayParameters(text3);
				((Control)DisplayMPOccultations.ListAndDisplay).Focus();
			}
			else
			{
				MessageBox.Show("File download failed", "File download error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
		}

		private static void GetAvailableCatalogues(ref string MessageText, ref string AvailableCatalogues)
		{
			if (Settings.Default.UCAC4_Path.Length > 0)
			{
				UCAC4Present = File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z500");
				MessageText = "A large number of stars will not be matched to a catalogue";
				AvailableCatalogues += "\r\n*  UCAC4";
			}
			if (Settings.Default.PPMXL_Path.Length > 0)
			{
				PPMXLpresent = File.Exists(Settings.Default.PPMXL_Path + "\\n00a.dat");
				MessageText = "A small number of stars will not be matched to a catalogue";
				AvailableCatalogues += "\r\n*  PPMXL";
			}
			if (Settings.Default.NOMAD_Path.Length > 0)
			{
				NOMADpresent = File.Exists(Settings.Default.NOMAD_Path + "\\100\\m1000.cat");
				MessageText = "A small number of stars will not be matched to a catalogue";
				AvailableCatalogues += "\r\n*  NOMAD";
			}
			if (AvailableCatalogues.Length == 0)
			{
				AvailableCatalogues += "\r\n*  None";
			}
			MessageText = "Catalogues available in Occult for matching to the RIO TNO predictions are:" + AvailableCatalogues + "\r\n" + MessageText + "\r\n\r\nDo you want to exclude unmatched stars from the output ?";
		}

		internal static bool Download_RIO_SourceFiles(bool JupSats)
		{
			string text = Utilities.AppPath + "\\Downloaded Files\\";
			Application.set_UseWaitCursor(true);
			bool flag = true;
			if (JupSats)
			{
				string fileName = "tableOccult_update.txt";
				string text2 = "RIO_Updates.dat";
				flag &= http.DownloadHTTP(Settings.Default.JupSats_Server, fileName, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false);
				fileName = "LOG.dat";
				text2 = "RIO_Update_Dates.dat";
				flag &= http.DownloadHTTP(Settings.Default.JupSats_Server, fileName, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false);
			}
			else
			{
				string fileName = "tableOccult.txt";
				string text2 = "RIO_Main.dat";
				if (!JupSats)
				{
					flag &= http.DownloadHTTP(Settings.Default.RIO_server, fileName, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false);
				}
				fileName = "tableOccult_update.txt";
				text2 = "RIO_Updates.dat";
				flag &= http.DownloadHTTP(Settings.Default.RIO_server, fileName, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false);
				fileName = "LOG.dat";
				text2 = "RIO_Update_Dates.dat";
				flag &= http.DownloadHTTP(Settings.Default.RIO_server, fileName, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false);
			}
			Application.set_UseWaitCursor(false);
			return flag;
		}

		private static void CreateUpdateList(string UpdateFile)
		{
			int num = 0;
			if (File.Exists(UpdateFile))
			{
				RUpdate.Clear();
				using StreamReader streamReader = new StreamReader(UpdateFile);
				do
				{
					RIO_Updates rIO_Updates = new RIO_Updates();
					rIO_Updates.ReadLine(streamReader.ReadLine(), num);
					RUpdate.Add(rIO_Updates);
					num++;
				}
				while (!streamReader.EndOfStream);
			}
			RIO_Updates.SortField = 1;
			RUpdate.Sort();
			for (int i = 0; i < RUpdate.Count - 1; i++)
			{
				if (RUpdate[i].ID == RUpdate[i + 1].ID && Math.Abs(RUpdate[i].JDevent - RUpdate[i + 1].JDevent) - 0.1 < 0.0)
				{
					if (RUpdate[i].MJD_update < RUpdate[i + 1].MJD_update)
					{
						RUpdate[i].Valid = false;
					}
					else
					{
						RUpdate[i + 1].Valid = false;
					}
				}
			}
			RIO_Updates.SortField = 0;
			RUpdate.Sort();
		}

		private static void Read_LuckyStar_RIO_File(string FileName, bool IsUpdateFile, int NumberOfObjects, string SourceID)
		{
			//IL_0778: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e3: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string orbitSource = "";
			string text2 = "";
			Path.GetFileName(FileName);
			UpdateCounter = -1;
			int num = 0;
			int num2 = 0;
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Converting RIO predictions");
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(NumberOfObjects + 1);
			int top;
			if (NumberOfObjects > 0)
			{
				((Control)pBar).Show();
				((Control)pBar).set_Left(top = 100);
				((Control)pBar).set_Top(top);
				((Control)pBar).Focus();
			}
			string text3 = File.ReadAllText(FileName);
			int num3 = 0;
			int num4 = 0;
			do
			{
				num4 = text3.IndexOf("Planete:", num4);
				if (num4 < 0)
				{
					break;
				}
				num3++;
				num4++;
			}
			while (num4 > 0);
			text3 = "";
			PBar pBar2 = new PBar();
			pBar2.pBarFTP.set_Minimum(0);
			pBar2.pBarFTP.set_Maximum(num3 + 1);
			pBar2.pBarFTP.set_Value(0);
			((Control)pBar2).Show();
			((Control)pBar2).set_Text("Converting " + num3 + " Objects");
			((Control)pBar2).set_Left(top = 70);
			((Control)pBar2).set_Top(top);
			using (StreamReader streamReader = new StreamReader(FileName))
			{
				DateTime lastWriteTime = new FileInfo(FileName).LastWriteTime;
				do
				{
					IDNumber = (NumOfMoons = (NumOfRings = 0));
					MAnom = (Perih = (node = (i = (e = (A = (Q = (Mag_H0 = 0.0)))))));
					MagG = 0.15;
					Mag_CoeffLogR = 5.0;
					AsteroidDiameter = 200.0;
					Epoch = "20000101.00000";
					MJDofPrediction = 0.0;
					do
					{
						text = streamReader.ReadLine()!.PadRight(60);
						if (text.Contains("Prediction Date:"))
						{
							int num5 = text.LastIndexOf(":");
							MJDofPrediction = double.Parse(text.Substring(num5 + 1).Trim());
						}
						if (text.Contains("Planete:"))
						{
							int num6 = text.LastIndexOf(":");
							orbitSource = SourceID;
							if (num6 > 0)
							{
								orbitSource = SourceID + text.Substring(num6 + 1).TrimEnd(Array.Empty<char>()).Replace(",", "_");
							}
							ProgressBar pBarFTP = pBar2.pBarFTP;
							top = pBarFTP.get_Value();
							pBarFTP.set_Value(top + 1);
						}
						if (!text.Contains("Radius (km)"))
						{
							continue;
						}
						if (double.TryParse(text.Substring(0, 10), out var result))
						{
							AsteroidDiameter = result * 2.0;
						}
						text2 = streamReader.ReadLine()!.PadRight(60);
						text2.Contains("29P");
						InvolvesMajorPlanet = false;
						InvolvesComet = false;
						InvolvesAsteroidMoon = false;
						string[] array = text2.Replace("".PadRight(8), " ").Replace("".PadRight(4), " ").Replace("".PadRight(3), " ")
							.Replace("  ", " ")
							.Replace("nal Des", "nal_Des")
							.Trim()
							.Replace(" ", ",")
							.Replace(",,", ",")
							.Split(new char[1] { ',' });
						IDforUpdateMatch = array[0];
						IDNum = array[1];
						if ((IDNum.Length == 0) & (IDforUpdateMatch.Length > 0))
						{
							IDNum = IDforUpdateMatch;
						}
						IDName = array[2];
						IDmoonID = "";
						PlanetID = "";
						if (IDNum.Substring(IDNum.Length - 1) == "P" && Information.IsNumeric((object)IDNum.Substring(0, IDNum.Length - 1)))
						{
							InvolvesComet = true;
							IDName = IDNum + "/" + IDName;
							IDNum = "0";
						}
						if (("CP".Contains(IDNum.Substring(0, 1)) & (IDNum.Length > 5)) && (Information.IsNumeric((object)IDNum.Substring(1, 4)) & !Information.IsNumeric((object)IDNum.Substring(1, 5))))
						{
							InvolvesComet = true;
							if (IDNum.Substring(1, 1) != "/")
							{
								IDNum = IDNum.Insert(1, "/").Trim();
							}
							IDName = IDNum + " " + IDName;
							IDNum = "0";
						}
						if (!InvolvesComet && IDNum.Substring(0, 1) == "P")
						{
							if (Information.IsNumeric((object)IDNum.Substring(1)) & (IDNum.Length == 4))
							{
								InvolvesMajorPlanet = true;
								PlanetID = IDNum.Substring(1);
							}
							else if (IDNum.Contains("M"))
							{
								int num7 = IDNum.IndexOf("M");
								PlanetID = IDNum.Substring(1, num7 - 1);
								IDmoonID = IDNum.Substring(num7 + 1);
								if (num7 == 2)
								{
									InvolvesMajorPlanet = true;
								}
								else
								{
									InvolvesAsteroidMoon = true;
								}
							}
						}
						if (!InvolvesComet & !InvolvesMajorPlanet & !InvolvesAsteroidMoon)
						{
							if (!int.TryParse(IDNum, out IDNumber))
							{
								IDNumber = 0;
							}
							if (IDNumber > 0)
							{
								AsteroidClass = AsteroidClassList.ClassOfAnAsteroid(IDNumber);
							}
							else
							{
								AsteroidClass = "";
							}
							if (IDNumber == 134340)
							{
								InvolvesMajorPlanet = true;
								PlanetID = "9";
								IDmoonID = IDmoonID.Replace(" / ", "").Trim();
							}
							else
							{
								IDName = IDmoonID + IDName;
							}
						}
						((Control)pBar2).set_Text("Converting " + num3 + " Objects     (" + IDNumber + ")  " + IDName);
						text = streamReader.ReadLine();
						text = streamReader.ReadLine();
						break;
					}
					while (!(text.Substring(1, 10) == " d  m year") && !streamReader.EndOfStream);
					if (streamReader.EndOfStream)
					{
						return;
					}
					GetFieldLocations(text);
					if (!InvolvesMajorPlanet & !InvolvesComet & !InvolvesAsteroidMoon)
					{
						if (Elements.MainAsteroids.AstElements.Count < 1)
						{
							Elements.MainAsteroids.Fill_AllAsteroids();
						}
						int num8 = -1;
						if (IDNumber > 0)
						{
							num8 = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(IDNumber);
						}
						if (num8 < 0)
						{
							if (IDName.Length > 4)
							{
								bool flag = Information.IsNumeric((object)IDName.Substring(0, 4));
								if (!flag)
								{
									MessageBox.Show("The following object has not been identified\r\n\r\n" + text2 + " [Diameter = " + AsteroidDiameter + " km]\r\n\r\nThe cause is likely to be \r\ni. your file of asteroid elements may not have asteroids as small as this asteroid. You should do a fresh conversion of asteroid orbital elements, using a smaller diameter.\r\nOR\r\nii. invalid data in one of the first two fields in the line. The error should be raised with LTE (Laboratoire Temps Espace), whichis a department in Paris Observatory", "Error in data", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
								}
								bool flag2 = IDName.Substring(4, 1) != " ";
								if (flag && flag2)
								{
									IDName = IDName.Insert(4, " ");
								}
								num8 = Elements.MainAsteroids.GetAsteroidRecord_fromName(IDName);
							}
							else
							{
								MessageBox.Show("The identification of the following object is apparently in error\r\n\r\n" + text2 + "\r\n\r\nThe cause is likely to be invalid data in one of the first two fields in the line. The error should be raised with IMCCE", "Error in data", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
							}
						}
						if (num8 >= 0)
						{
							if (IDNumber == 0)
							{
								IDNumber = Elements.MainAsteroids.AstElements[num8].IDNumber;
							}
							Epoch = Elements.MainAsteroids.AstElements[num8].EpochYear + Elements.MainAsteroids.AstElements[num8].EpochMonth.ToString().PadLeft(2) + Elements.MainAsteroids.AstElements[num8].EpochDay.ToString().PadLeft(2);
							MAnom = Elements.MainAsteroids.AstElements[num8].Meananomaly;
							Perih = Elements.MainAsteroids.AstElements[num8].Perihelion;
							node = Elements.MainAsteroids.AstElements[num8].Node;
							i = Elements.MainAsteroids.AstElements[num8].I;
							e = Elements.MainAsteroids.AstElements[num8].E;
							A = Elements.MainAsteroids.AstElements[num8].A;
							Q = A * (1.0 - e);
							Mag_H0 = Elements.MainAsteroids.AstElements[num8].H0;
							Mag_CoeffLogR = 5.0;
							if (MagG == 0.0)
							{
								MagG = Elements.MainAsteroids.AstElements[num8].G_phaseCoeff;
							}
							if (AsteroidDiameter == 200.0)
							{
								AsteroidDiameter = Elements.MainAsteroids.AstElements[num8].Diameter_Mean;
							}
							NumOfRings = Elements.MainAsteroids.AstElements[num8].Num_Rings;
							NumOfMoons = Elements.MainAsteroids.AstElements[num8].Num_Moons;
						}
					}
					text = streamReader.ReadLine();
					if (NumberOfObjects > 0)
					{
						pBar.pBarFTP.set_Value(num);
						Application.DoEvents();
					}
					Utilities.SetAnimatedCursor(ref num2);
					do
					{
						text = streamReader.ReadLine();
						if (text.Contains("------"))
						{
							break;
						}
						if (MJDofPrediction == 0.0)
						{
							MJDofPrediction = Utilities.JD_from_Date(lastWriteTime.Year, lastWriteTime.Month, (double)lastWriteTime.Day + (double)lastWriteTime.Hour / 24.0) - 2400000.5;
						}
						if (IsUpdateFile)
						{
							UpdateCounter++;
							if (!RUpdate[UpdateCounter].Valid)
							{
								continue;
							}
						}
						Convert_LuckyStar_RIO_Prediction(text, IsUpdateFile, MJDofPrediction, orbitSource);
						EventCounter++;
					}
					while (!streamReader.EndOfStream);
					num++;
				}
				while (!streamReader.EndOfStream);
				Cursor.set_Current(Cursors.get_Default());
			}
			((Form)pBar).Close();
			((Form)pBar2).Close();
		}

		private static void GetFieldLocations(string Header)
		{
			pRAstar = Header.IndexOf("ra___dec___J2000_candidate");
			pDecstar = pRAstar + 14;
			pRAObj = Header.IndexOf("ra___dec___J2000_ephemeris");
			pDecObj = pRAObj + 14;
			pClosestSepn = Header.IndexOf("C/A") + 2;
			pPAvel = Header.IndexOf("P/A") + 2;
			pVel = Header.IndexOf("vel") + 1;
			pDelta = Header.IndexOf("Delta") + 2;
			pRmag = Header.IndexOf("G*") + 1;
			if (pRmag < 1)
			{
				pRmag = Header.IndexOf("R*") + 1;
			}
			pErrRA = Header.IndexOf("e_ra") + 3;
			pErrDec = Header.IndexOf("e_de") + 3;
			pRUWE = Header.IndexOf("RUWE") + 2;
			pDuplicate = Header.IndexOf("Dup") + 2;
		}

		private static void Convert_LuckyStar_RIO_Prediction(string PredictionLine, bool IsUpdateFile, double MJDofPrediction, string OrbitSource)
		{
			//IL_0772: Unknown result type (might be due to invalid IL or missing references)
			if (PredictionLine.Trim().Length < 10 || PredictionLine.Contains("----"))
			{
				return;
			}
			double num = 0.0;
			double num2 = 0.0;
			try
			{
				if (!int.TryParse(PredictionLine.Substring(0, 2), out Arg3))
				{
					Arg3 = 0;
				}
				Day = Arg3;
				if (!int.TryParse(PredictionLine.Substring(3, 2), out Arg3))
				{
					Arg3 = 0;
				}
				MinorPlanetConvertRIO.Month = Arg3;
				if (!int.TryParse(PredictionLine.Substring(6, 4), out Arg3))
				{
					Arg3 = 0;
				}
				MinorPlanetConvertRIO.Year = Arg3;
				if (!int.TryParse(PredictionLine.Substring(12, 2), out Arg3))
				{
					Arg3 = 0;
				}
				Hr = Arg3;
				if (!int.TryParse(PredictionLine.Substring(15, 2), out Arg3))
				{
					Arg3 = 0;
				}
				Min = Arg3;
				if (!double.TryParse(PredictionLine.Substring(18, 3), out Arg2))
				{
					Arg2 = 0.0;
				}
				Sec = Arg2;
				num = Utilities.JD_from_Date(MinorPlanetConvertRIO.Year, MinorPlanetConvertRIO.Month, Day);
				num2 = (double)Hr + (double)Min / 60.0 + Sec / 3600.0;
				if (ExcludeOldEvents & (num < JD_OneMonthAgo))
				{
					return;
				}
				raSTAR = PredictionLine.Substring(pRAstar, 13);
				RAStar = (double.Parse(raSTAR.Substring(0, 2)) + double.Parse(raSTAR.Substring(3, 2)) / 60.0 + double.Parse(raSTAR.Substring(6)) / 3600.0) * 15.0 / (180.0 / Math.PI);
				decSTAR = PredictionLine.Substring(pDecstar, 14);
				if (decSTAR.IndexOf(".") == 10)
				{
					decSTAR = decSTAR.Remove(6, 1);
				}
				DecStar = (double.Parse(decSTAR.Substring(1, 2)) + double.Parse(decSTAR.Substring(4, 2)) / 60.0 + double.Parse(decSTAR.Substring(7)) / 3600.0) / (180.0 / Math.PI);
				if (decSTAR.Substring(0, 1) == "-")
				{
					DecStar = 0.0 - DecStar;
				}
				raOBJECT = PredictionLine.Substring(pRAObj, 13);
				RAObject = (double.Parse(raOBJECT.Substring(0, 2)) + double.Parse(raOBJECT.Substring(3, 2)) / 60.0 + double.Parse(raOBJECT.Substring(6)) / 3600.0) * 15.0 / (180.0 / Math.PI);
				decOBJECT = PredictionLine.Substring(pDecObj, 14);
				if (decOBJECT.IndexOf(".") == 10)
				{
					decOBJECT = decOBJECT.Remove(6, 1);
				}
				DecObject = (double.Parse(decOBJECT.Substring(1, 2)) + double.Parse(decOBJECT.Substring(4, 2)) / 60.0 + double.Parse(decOBJECT.Substring(7)) / 3600.0) / (180.0 / Math.PI);
				if (decSTAR.Substring(0, 1) == "-")
				{
					DecObject = 0.0 - DecObject;
				}
				if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pVel)..PredictionLine.IndexOf(" ", pVel)], out Arg2))
				{
					Arg2 = 0.0;
				}
				Velocity = Arg2;
				if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pClosestSepn)..PredictionLine.IndexOf(" ", pClosestSepn)], out Arg2))
				{
					Arg2 = 0.0;
				}
				ClosestSepn = Arg2;
				if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pPAvel)..PredictionLine.IndexOf(" ", pPAvel)], out Arg2))
				{
					Arg2 = 0.0;
				}
				PAofConjunction = Arg2;
				int num3 = PredictionLine.LastIndexOf(" ", pDelta);
				int num4 = PredictionLine.IndexOf(" ", pDelta) - num3;
				if (num4 == 0)
				{
					num4 = PredictionLine.IndexOf(" ", pDelta + 1) - num3;
				}
				if (!double.TryParse(PredictionLine.Substring(num3, num4), out Arg2))
				{
					Arg2 = 0.0;
				}
				Delta = Arg2;
				if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pRmag)..PredictionLine.IndexOf(" ", pRmag)], out Arg2))
				{
					Arg2 = 0.0;
				}
				MinorPlanetConvertRIO.Mv = Arg2 - 2.5 * Math.Log10(Math.Abs(Velocity) / 20.0);
				if (pErrRA > 2)
				{
					if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pErrRA)..PredictionLine.IndexOf(" ", pErrRA)], out Arg2))
					{
						Arg2 = 0.0;
					}
					ErrRA = Arg2;
					if (ErrRA > 300.0)
					{
						ErrRA = 300.0;
					}
					num3 = PredictionLine.LastIndexOf(" ", pErrDec);
					num4 = PredictionLine.IndexOf(" ", pErrDec) - num3;
					if (num4 < 0)
					{
						num4 = PredictionLine.Length - num3;
					}
					if (!double.TryParse(PredictionLine.Substring(num3, num4), out Arg2))
					{
						Arg2 = 0.0;
					}
					ErrDec = Arg2;
					if (ErrDec > 300.0)
					{
						ErrDec = 300.0;
					}
				}
				else
				{
					ErrRA = (ErrDec = 300.0);
				}
				if (pRUWE > 2)
				{
					if (!double.TryParse(PredictionLine[PredictionLine.LastIndexOf(" ", pRUWE)..PredictionLine.IndexOf(" ", pRUWE)], out Arg2))
					{
						Arg2 = 0.0;
					}
					RUWE = Arg2;
				}
				else
				{
					RUWE = -1.0;
				}
				if (pDuplicate > 2)
				{
					num3 = PredictionLine.LastIndexOf(" ", pDuplicate);
					num4 = PredictionLine.IndexOf(" ", pDuplicate) - num3;
					if (num4 < 0)
					{
						num4 = PredictionLine.Length - num3;
					}
					if (!int.TryParse(PredictionLine.Substring(num3, num4), out Arg3))
					{
						Arg3 = -1;
					}
					Duplicate = Arg3;
				}
				else
				{
					Duplicate = -1;
				}
			}
			catch
			{
				MessageBox.Show("Error reading LuckyStar file at line \r\n" + PredictionLine + "\r\n\r\nEdit the file in a text editor to remove the error, and run conversion again using the downloaded file.", "File read error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			if (!IsUpdateFile)
			{
				for (int i = 0; i < RUpdate.Count; i++)
				{
					if (((IDforUpdateMatch == RUpdate[i].ID) & RUpdate[i].Valid) && Math.Abs(num + num2 / 24.0 - RUpdate[i].JDevent) < 0.1)
					{
						return;
					}
				}
			}
			double delta_MeanAnomaly = 0.0;
			double delta_Perihelion = 0.0;
			double delta_Node = 0.0;
			double delta_i = 0.0;
			double delta_e = 0.0;
			double delta_a = 0.0;
			double delta_q = 0.0;
			double num5 = 25.0;
			double num6 = 25.0;
			double Mb = 25.0;
			double Mv = 25.0;
			double Mr = 25.0;
			string text = "";
			if (UCAC4Present)
			{
				text = UCAC4.UCAC4numFromCoords(RAStar, DecStar, (double)MinorPlanetConvertRIO.Year + (double)MinorPlanetConvertRIO.Month / 12.0 - 2000.0, out Mb, out Mv, out Mr);
				if (text != "")
				{
					if (num5 == 25.0)
					{
						num5 = Mb;
					}
					if (MinorPlanetConvertRIO.Mv == 25.0)
					{
						MinorPlanetConvertRIO.Mv = Mv;
					}
					if (num6 == 25.0)
					{
						num6 = Mr;
					}
				}
			}
			if (text == "")
			{
				if (PPMXLpresent)
				{
					text = PPMXL.PPMXLnumFromCoords(RAStar, DecStar, (double)MinorPlanetConvertRIO.Year + (double)MinorPlanetConvertRIO.Month / 12.0 - 2000.0, out Mb, out Mv, out Mr);
				}
				if (text != "")
				{
					if (num5 == 25.0)
					{
						num5 = Mb;
					}
					if (MinorPlanetConvertRIO.Mv == 25.0)
					{
						MinorPlanetConvertRIO.Mv = Mv;
					}
					if (num6 == 25.0)
					{
						num6 = Mr;
					}
				}
			}
			if (text == "")
			{
				if (NOMADpresent)
				{
					text = NOMAD.NOMADnumFromCoords(RAStar, DecStar, (double)MinorPlanetConvertRIO.Year + (double)MinorPlanetConvertRIO.Month / 12.0 - 2000.0, out Mb, out Mv, out Mr);
				}
				if (text != "")
				{
					if (num5 == 25.0)
					{
						num5 = Mb;
					}
					if (MinorPlanetConvertRIO.Mv == 25.0)
					{
						MinorPlanetConvertRIO.Mv = Mv;
					}
					if (num6 == 25.0)
					{
						num6 = Mr;
					}
				}
			}
			if (SuppressNonMatchedStars & (text == ""))
			{
				return;
			}
			if (text == "")
			{
				text = "Seq no " + EventCounter;
			}
			double Magnitude = 20.0;
			double num7 = 0.0;
			double num8 = 1.0;
			int num9 = 0;
			int result = 0;
			int result2 = 0;
			double RA = 0.0;
			double Dec = 0.0;
			double Elongation = 0.0;
			double PhaseAngle_deg = 0.0;
			double illumination = 100.0;
			double PAlimb_deg = 0.0;
			double Planetocentric_Latitude_deg = 0.0;
			double PAPole_deg = 0.0;
			double TrueDistance;
			double Planetocentric_SunLat_deg;
			if (InvolvesMajorPlanet)
			{
				if (!int.TryParse(PlanetID, out result))
				{
					result = 0;
				}
				if (!int.TryParse(IDmoonID, out result2))
				{
					result2 = 0;
				}
				if (result > 0)
				{
					float MoonDiaKm = 1f;
					float Mag = 20f;
					string MoonName = "";
					if (result2 > 0 && result != 9)
					{
						Satellites.SatelliteCoordinates(num + num2 / 24.0, result, result2, ref MoonName, ref MoonDiaKm, ref RA, ref Dec, ref Mag);
						Magnitude = Mag;
					}
					else
					{
						Utilities.PlanetGeocentric(num + num2 / 24.0, result, 1E-05, 2, physicalFlag: true, out var _, out var _, out var _, out RA, out Dec, out TrueDistance, out var _, out Magnitude, out Elongation, out var _, out PhaseAngle_deg, out illumination, out PAlimb_deg, out Planetocentric_Latitude_deg, out PAPole_deg, out Planetocentric_SunLat_deg);
					}
				}
			}
			else if (Q != 0.0)
			{
				double osculatingDate = Utilities.JD_from_Date(int.Parse(Epoch.Substring(0, 4)), int.Parse(Epoch.Substring(4, 2)), double.Parse(Epoch.Substring(6)));
				Utilities.PositionfromElements(num + num2 / 24.0, 0.0, 0.0, 0.0, 0.0, osculatingDate, MAnom / (180.0 / Math.PI), Q, e, Perih / (180.0 / Math.PI), node / (180.0 / Math.PI), MinorPlanetConvertRIO.i / (180.0 / Math.PI), Mag_H0, MagG + 0.0001, Mag_CoeffLogR, 1E-05, out RA, out Dec, out var _, out TrueDistance, out Magnitude, out Elongation, out PhaseAngle_deg);
			}
			double num10 = ClosestSepn / 8.794143836182533 * Delta;
			double xatMin = num10 * Math.Sin(PAofConjunction / (180.0 / Math.PI));
			double yatMin = num10 * Math.Cos(PAofConjunction / (180.0 / Math.PI));
			double num11 = Velocity / 6378.137 * 3600.0;
			if (PAofConjunction > 270.0)
			{
				PAofConjunction -= 360.0;
			}
			if (PAofConjunction > 90.0)
			{
				PAofConjunction -= 180.0;
			}
			double num12 = num11 * Math.Cos(PAofConjunction / (180.0 / Math.PI));
			double num13 = (0.0 - num11) * Math.Sin(PAofConjunction / (180.0 / Math.PI));
			double num14 = 8.794143836182533 / Delta * num12 / Math.Cos(DecStar);
			double num15 = 8.794143836182533 / Delta * num13;
			double num16 = ErrRA / 1000.0;
			double num17 = ErrDec / 1000.0;
			double errorEllipsePA = 90.0;
			if (num17 > num16)
			{
				errorEllipsePA = 0.0;
			}
			string errorBasis = "";
			if (OrbitSource.ToUpper().Contains("RIO"))
			{
				errorBasis = "Asteroid only";
			}
			else if (OrbitSource.ToUpper().Contains("LUCKY"))
			{
				errorBasis = ((ErrRA != 99.0) ? "Star+Asteroid" : "Nominal");
			}
			if (num16 == 0.0)
			{
				num16 = (num17 = 0.1);
				ErrRA = (ErrDec = 100.0);
				errorBasis = "Assumed";
			}
			int num18 = 0;
			num18 = Interferometric_Plus_WDS.StarIsInDoubleCats(RAStar * (180.0 / Math.PI) / 15.0, DecStar * (180.0 / Math.PI));
			if (Interferometric_Plus_WDS.IsInAAVSO(RAStar * (180.0 / Math.PI) / 15.0, DecStar * (180.0 / Math.PI)).Length > 1)
			{
				num18 += 4;
			}
			bool flag = false;
			if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			flag = Kepler2.StarInKepler2(RAStar * (180.0 / Math.PI), DecStar * (180.0 / Math.PI));
			_ = new double[10];
			_ = new double[10];
			_ = new string[10] { "", "", "", "", "", "", "", "", "", "" };
			List<int> list = new List<int>();
			if (!InvolvesMajorPlanet)
			{
				BinaryAsteroids.GetAsteroidRecord_fromNumberName(IDNumber, IDName, list);
				_ = list.Count;
			}
			num9 = 0;
			double num19 = AsteroidDiameter + (double)num9 / 3600000.0 / (180.0 / Math.PI) * Delta * 149600000.0;
			double num20 = num19 / AsteroidDiameter;
			double num21 = Velocity / 6378.137 * 3600.0;
			double maxDurn = Math.Abs(num19 * num20 / num21 * 3600.0 / 6378.137);
			double num22 = (Utilities.SiderealTime_deg(num, Apparent: true) + 360.98568 * num2 / 24.0) % 360.0;
			double RA2 = RAStar;
			double Dec2 = DecStar;
			Utilities.PrecessFromJ2000(num, use2006values_Not1976: false, ref RA2, ref Dec2);
			double num23;
			for (num23 = 180.0 / Math.PI * RA2 - num22; num23 > 180.0; num23 -= 360.0)
			{
			}
			for (; num23 < -180.0; num23 += 360.0)
			{
			}
			Utilities.QuickPlanet(num + num2 / 24.0, 3, EquinoxOfDate: true, out var RA3, out var Dec3, out var _);
			double num24;
			for (num24 = RA3 * (180.0 / Math.PI) - num22; num24 > 180.0; num24 -= 360.0)
			{
			}
			for (; num24 < -180.0; num24 += 360.0)
			{
			}
			double subSolarLatitude_OfDate_deg = Dec3 * (180.0 / Math.PI);
			double RA4 = 0.0;
			double Dec4 = 0.0;
			double GeocentricDistance2 = 0.0;
			double Distance = 0.0;
			double num25 = 0.0;
			Utilities.QuickMoon(num + num2 / 24.0, out var RA5, out var Dec5, out var _, out var _, out var _);
			Utilities.QuickPlanet(num + num2 / 24.0, 3, EquinoxOfDate: true, out RA4, out Dec4, out GeocentricDistance2);
			Utilities.Distance(RA5, Dec5, RA4, Dec4, out var Distance2, out Planetocentric_SunLat_deg);
			if (Dec2 != 0.0)
			{
				Utilities.Distance(RA5, Dec5, RA2, Dec2, out Distance, out Planetocentric_SunLat_deg);
			}
			else
			{
				Utilities.Distance(RA5, Dec5, RAStar, DecStar, out Distance, out Planetocentric_SunLat_deg);
			}
			Distance *= 180.0 / Math.PI;
			num25 = 50.0 * (1.0 - Math.Cos(Distance2));
			double num26 = 0.0;
			if (num7 > 0.0)
			{
				num26 = (num8 + (num20 - 1.0)) / num20;
			}
			else if ((ErrRA > 0.0) & (ErrDec > 0.0))
			{
				double num27 = (PAofConjunction + 90.0) / (180.0 / Math.PI);
				double num28 = Math.Sqrt(ErrDec / 1000.0 * Math.Sin(num27) * ErrDec / 1000.0 * Math.Sin(num27) + ErrRA / 1000.0 * Math.Cos(num27) * ErrRA / 1000.0 * Math.Cos(num27)) / (AsteroidDiameter / 6378.137 * 8.794143836182533 / Delta);
				num26 = 1.0 + num28 + (num20 - 1.0) / num20;
			}
			else
			{
				double num29 = ((!(AsteroidDiameter > 0.0)) ? (1.0 + num16 / (0.0006893975338082682 / Delta)) : (1.0 + num16 / (AsteroidDiameter / 6378.137 * 8.794143836182533 / Delta)));
				num26 = (num29 + (num20 - 1.0)) / num20;
			}
			OccultationElements occultationElements = new OccultationElements();
			Utilities.Date_from_MJD(MJDofPrediction, out var Year, out var Month, out var day);
			string text2 = Year + "-" + Utilities.ShortMonths[Month] + "-" + ((int)day).ToString().PadLeft(2, '0');
			occultationElements.OrbitSource = OrbitSource + ":" + text2;
			occultationElements.PredictionMJD = MJDofPrediction;
			occultationElements.IsAsteroid = !InvolvesMajorPlanet;
			if (InvolvesMajorPlanet)
			{
				occultationElements.ObjectNumber = "P" + result + "M" + result2.ToString().PadLeft(2, '0');
				occultationElements.Planet = result;
				occultationElements.PlanetaryMoon = result2;
				occultationElements.IsPlanetaryMoon = result2 > 0;
				occultationElements.IsPlanet = result > 0 && result2 < 1;
				occultationElements.ObjectName = IDName;
			}
			else if (InvolvesComet)
			{
				occultationElements.ObjectNumber = IDNum;
				occultationElements.ObjectName = IDName;
			}
			else if (InvolvesAsteroidMoon)
			{
				occultationElements.ObjectNumber = IDNum;
				occultationElements.ObjectName = IDName + " **";
			}
			else if (IDNumber == 0)
			{
				occultationElements.ObjectNumber = "";
				occultationElements.ObjectName = IDName;
			}
			else
			{
				occultationElements.ObjectNumber = IDNumber.ToString();
				occultationElements.ObjectName = IDName;
			}
			occultationElements.EventYear = MinorPlanetConvertRIO.Year;
			occultationElements.EventMonth = MinorPlanetConvertRIO.Month;
			occultationElements.EventDay = Day;
			occultationElements.EventJD = num;
			occultationElements.MidTime_Hrs = num2;
			occultationElements.XatMin = xatMin;
			occultationElements.dX = num12;
			occultationElements.d2X = 0.0;
			occultationElements.d3X = 0.0;
			occultationElements.YatMin = yatMin;
			occultationElements.dY = num13;
			occultationElements.d2Y = 0.0;
			occultationElements.d3Y = 0.0;
			occultationElements.StarCatName = text;
			occultationElements.RA_Star2000 = RAStar;
			occultationElements.Dec_Star2000 = DecStar;
			double RA6 = RAStar;
			double Dec6 = DecStar;
			Utilities.ApparentStarPosition(ref RA6, ref Dec6, 0.0, 0.0, 2000, num, use2006values_Not1976: false);
			occultationElements.RA_Star_Apparent = RA6;
			occultationElements.Dec_Star_Apparent = Dec6;
			occultationElements.UsesApparentStar = false;
			occultationElements.MagAsteroid = Magnitude;
			if ((num5 == 0.0 || num5 > 20.0) && num6 > 0.0 && num6 < 20.0)
			{
				num5 = (1.54 * MinorPlanetConvertRIO.Mv - num6) / 0.54;
			}
			else if (num5 > 0.0 && num5 < 20.0 && (num6 == 0.0 || num6 > 20.0))
			{
				num6 = 1.54 * MinorPlanetConvertRIO.Mv - 0.54 * num5;
			}
			occultationElements.mB = num5;
			occultationElements.mV = MinorPlanetConvertRIO.Mv;
			occultationElements.mR = num6;
			double ExtraMagG = 30.0;
			double ExtraMagR = 30.0;
			int AllStars = 0;
			int BrightStars = 0;
			Gaia.GetNearbyStarsFromCoords(text, RAStar, DecStar, MinorPlanetConvertRIO.Mv, Utilities.BesselianYear(num) - 2000.0, 15.0, out ExtraMagG, out ExtraMagR, out BrightStars, out AllStars, out var _);
			occultationElements.NearbyStars_Bright = BrightStars;
			occultationElements.NearbyStars_All = AllStars;
			occultationElements.NearbyStars_MagAdjusted = ExtraMagG < 20.0 || ExtraMagR < 20.0;
			occultationElements.CombinedMagnitudeV = Utilities.CombinedMagnitude(ExtraMagG, Magnitude);
			double num30 = Utilities.CombinedMagnitude(ExtraMagG, Magnitude);
			double num31 = Utilities.CombinedMagnitude(num30, MinorPlanetConvertRIO.Mv);
			occultationElements.MagDropV = num30 - num31;
			if (num6 > 0.0 && num6 < 20.0)
			{
				double num32 = Utilities.CombinedMagnitude(ExtraMagR, Magnitude - 0.45);
				double num33 = Utilities.CombinedMagnitude(num32, num6);
				occultationElements.MagDropR = num32 - num33;
			}
			else
			{
				occultationElements.MagDropR = 0.0;
			}
			occultationElements.DoubleStarCode = num18;
			occultationElements.IsKepler2Star = flag;
			occultationElements.StarReliability = RUWE;
			occultationElements.DuplicateSource = Duplicate;
			occultationElements.NoGaiaPM = 0;
			occultationElements.GaiaUCAC4PM = 0;
			occultationElements.AsteroidDiameter = AsteroidDiameter;
			occultationElements.AsteroidShadowDiameter_Penumbral = num19;
			occultationElements.AsteroidDiameterArcSec = occultationElements.AsteroidDiameter / 6378.137 * 8.794143836182533 / Delta;
			occultationElements.MaxDurn = maxDurn;
			occultationElements.DistAsteroid = Delta;
			occultationElements.hourly_dRA = num14 / (180.0 / Math.PI);
			occultationElements.hourly_dDec = num15 / (180.0 / Math.PI);
			occultationElements.NumAsteroidRings = NumOfRings;
			occultationElements.NumAsteroidMoons = NumOfMoons;
			occultationElements.AsteroidClass = AsteroidClass;
			occultationElements.SubstellarLongitude_J2000_deg = num23;
			occultationElements.SubstellarLatitude_J2000_deg = DecStar * (180.0 / Math.PI);
			occultationElements.SubstellarLongitude_OfDate_deg = num23 + (RA6 - RAStar) * (180.0 / Math.PI);
			occultationElements.SubstellarLatitude_OfDate_deg = DecStar * (180.0 / Math.PI) + (Dec6 - DecStar) * (180.0 / Math.PI);
			occultationElements.SubSolarLongitude_OfDate_deg = num24;
			occultationElements.SubSolarLatitude_OfDate_deg = subSolarLatitude_OfDate_deg;
			occultationElements.SolarElongation = Math.Acos(Math.Sin(occultationElements.SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Sin(occultationElements.SubstellarLatitude_J2000_deg / (180.0 / Math.PI)) + Math.Cos(occultationElements.SubSolarLatitude_OfDate_deg / (180.0 / Math.PI)) * Math.Cos(occultationElements.SubstellarLatitude_J2000_deg / (180.0 / Math.PI)) * Math.Cos((occultationElements.SubstellarLongitude_J2000_deg - occultationElements.SubSolarLongitude_OfDate_deg) / (180.0 / Math.PI))) * (180.0 / Math.PI);
			if (!InvolvesMajorPlanet & !InvolvesComet)
			{
				occultationElements.EpochYear = int.Parse(Epoch.Substring(0, 4));
				occultationElements.EpochMonth = int.Parse(Epoch.Substring(4, 2));
				occultationElements.EpochDay = double.Parse(Epoch.Substring(6));
				occultationElements.MeanAnomaly = MAnom / (180.0 / Math.PI);
				occultationElements.Perihelion = Perih / (180.0 / Math.PI);
				occultationElements.Node = node / (180.0 / Math.PI);
				occultationElements.i = MinorPlanetConvertRIO.i / (180.0 / Math.PI);
				occultationElements.e = e;
				occultationElements.a = A;
				occultationElements.q = Q;
				occultationElements.MagConst_H0 = Mag_H0;
				occultationElements.MagCoeff_logR = Mag_CoeffLogR;
				occultationElements.MagSlopeConst_G = MagG;
				occultationElements.delta_MeanAnomaly = delta_MeanAnomaly;
				occultationElements.delta_Perihelion = delta_Perihelion;
				occultationElements.delta_Node = delta_Node;
				occultationElements.delta_i = delta_i;
				occultationElements.delta_e = delta_e;
				occultationElements.delta_a = delta_a;
				occultationElements.delta_q = delta_q;
			}
			occultationElements.PABrightLimb = PAlimb_deg;
			occultationElements.PlanetPhaseAngle = PhaseAngle_deg;
			occultationElements.PlanetocentricEarthDec = Planetocentric_Latitude_deg;
			occultationElements.PAPlanetPole = PAPole_deg;
			occultationElements.ErrorBasis = errorBasis;
			occultationElements.ErrorEllipseMajorAxis = num16;
			occultationElements.ErrorEllipseMinorAxis = num17;
			occultationElements.ErrorEllipsePA = errorEllipsePA;
			occultationElements.Error_AsIncreaseInPathWidths = num26;
			if (num7 > 0.0)
			{
				occultationElements.KnownSigma_StarAsterPosns = num7;
			}
			else
			{
				occultationElements.KnownSigma_StarAsterPosns = num16;
			}
			occultationElements.StarMoonElongation_deg = Distance;
			occultationElements.MoonPhase_percent = num25;
			occultationElements.AsteroidMoons.Clear();
			for (int j = 0; j < NumOfMoons; j++)
			{
				BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
				binaryAsteroidElements = BinaryAsteroids.BinElements[list[j]];
				occultationElements.AsteroidMoons.Add(binaryAsteroidElements);
			}
			string text3 = text.Trim().PadLeft(6).Replace(' ', '_');
			occultationElements.EventID = string.Format("{0,-15}", Utilities.Date_from_JD(num) + "_" + text3.Substring(text3.Length - 5, 5) + IDName.PadRight(2).Substring(1, 1));
			DisplayMPOccultations.OccElements.Add(occultationElements);
		}

		internal static bool ShowLuckyStarMessage(bool Reminder)
		{
			//IL_000a: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Invalid comparison between Unknown and I4
			string text = "";
			MessageBoxButtons val;
			if (Reminder)
			{
				val = (MessageBoxButtons)0;
				text += "REMINDER\r\n\r\n";
			}
			else
			{
				val = (MessageBoxButtons)4;
			}
			text += "By using this Lucky Star prediction you understand that:\r\n\r\n* This was made available to you thanks to cooperation\r\n  between Occult, OccultWatcher and the 'Lucky Star' project.\r\n\r\n* The Lucky Star project is a ERC Advanced Grant led by\r\n  Bruno Sicardy at Paris Observatory / Univ. Pierre et Marie\r\n  Curie. The Lucky Star project studies small bodies of\r\n  the solar system through stellar occultations.\r\n\r\n* Years of man power and financial resources are applied to\r\n  provide precise prediction of stellar occultations by\r\n  Centaurs and TNOs.\r\n\r\n* Evaluation of the Lucky Star project depends upon the\r\n  publication of successful observations.\r\n\r\n* Without harming the efforts/credits of any one, the Lucky\r\n  Star group wants to collaborate with the analysis and\r\n  possible publications of any detected event from the Lucky\r\n  Star feed.\r\n\r\n* Taking the above into account, whenever a Lucky Star event\r\n  is detected, contact\r\n     bruno.sicardy(at)obspm.fr\r\n     josselin.desmars(at)obspm.fr\r\n  to continue this collaborative work.\r\n\r\n\r\n";
			if (!Reminder)
			{
				text += "Processing will only continue if you agree to collaborate with\r\nthe Lucky Star group with any observations.\r\n\r\nDO YOU AGREE TO COLLABORATE WITH THE LUCKY STAR\r\nGROUP WITH ANY OBSERVATIONS MADE OF EVENTS IN \r\nTHIS FEED?";
			}
			if ((int)MessageBox.Show(text, "Right to use the LUCKY STAR predictions", val, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			return true;
		}

		public static void DownloadConvert_LuckyStar(bool ExcludeOld)
		{
			//IL_009d: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a3: Invalid comparison between Unknown and I4
			//IL_0207: Unknown result type (might be due to invalid IL or missing references)
			//IL_020d: Invalid comparison between Unknown and I4
			//IL_020f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_0238: Unknown result type (might be due to invalid IL or missing references)
			//IL_023e: Invalid comparison between Unknown and I4
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			if (!ShowLuckyStarMessage(Reminder: false))
			{
				return;
			}
			string text = Utilities.AppPath + "\\Downloaded Files\\";
			string text2 = Interaction.InputBox("Specify the prediction year", "Prediction year", DateTime.Now.Year.ToString(), 0, 0);
			if (text2.Length == 0)
			{
				return;
			}
			int.TryParse(text2, out var result);
			DateTime dateTime = DateTime.Now.ToUniversalTime();
			int num = result - dateTime.Year;
			if ((num > 3 || num < -1) && (int)MessageBox.Show("The year you specified (" + text2 + ") appears to be incorrect as it is more than 1 year before, or 3 years after, the present year.\r\n\r\nDo you want to continue anyway?", "Wrong Year specified", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			ExcludeOldEvents = ExcludeOld;
			DateTime dateTime2 = dateTime.AddMonths(-1);
			JD_OneMonthAgo = Utilities.JD_from_Date(dateTime2.Year, dateTime2.Month, dateTime2.Day);
			string text3 = "_" + dateTime.Year + dateTime.Month.ToString().PadLeft(2, '0') + dateTime.Day.ToString().PadLeft(2, '0');
			string fileName = "list_objects.txt";
			string text4 = text + "\\LuckyStarObjects" + text2 + ".dat";
			string text5 = text + "\\PartFile.dat";
			string text6 = text + "LuckyStarObjects" + text2 + text3 + ".dat";
			string text7 = Utilities.AppPath + "\\Generated Files\\LuckyStar" + text2 + text3 + ".xml";
			string text8 = "";
			string MessageText = "";
			string AvailableCatalogues = "";
			GetAvailableCatalogues(ref MessageText, ref AvailableCatalogues);
			if (MinorPlanetOccultationElements.BinaryElements.Count < 1)
			{
				MinorPlanetOccultationElements.CreateBinaryElementList();
			}
			if (NOMADpresent)
			{
				NOMAD.InitialiseNOMAD();
			}
			if (PPMXLpresent)
			{
				PPMXL.InitialisePPMXL();
			}
			int num2 = 0;
			int num3 = 0;
			if ((int)MessageBox.Show("Do you want to use a previously downloaded LuckyStarFile?", "Use previous download", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				OpenFileDialog val = new OpenFileDialog();
				((FileDialog)val).set_InitialDirectory(text);
				((FileDialog)val).set_FileName("LuckyStarObjects*.dat");
				((FileDialog)val).set_Title("Select Downloaded LuckyStarFile");
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				text6 = ((FileDialog)val).get_FileName();
			}
			else
			{
				if (!http.DownloadHTTP(Settings.Default.LuckyStar + text2, fileName, text4, unzip: false, gunzip: false, ShowMessages: false))
				{
					MessageBox.Show("The file listing Lucky Star objects for the year " + text2 + " has not been downloaded. Presumably it is not yet available.\r\n\r\n As a result, Lucky Star predictions for the " + text2 + " cannot be processed.", "Failed download", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
					return;
				}
				using (StreamReader streamReader = new StreamReader(text4))
				{
					do
					{
						streamReader.ReadLine();
						num2++;
					}
					while (!streamReader.EndOfStream);
				}
				using (StreamWriter streamWriter = new StreamWriter(text6))
				{
					PBar pBar = new PBar();
					num3 = 0;
					pBar.pBarFTP.set_Minimum(0);
					pBar.pBarFTP.set_Maximum(num2 + 1);
					((Control)pBar).Show();
					((Control)pBar).set_Text("Downloading " + num2 + " Lucky Star files");
					int top;
					((Control)pBar).set_Left(top = 70);
					((Control)pBar).set_Top(top);
					using (StreamReader streamReader2 = new StreamReader(text4))
					{
						do
						{
							pBar.pBarFTP.set_Value(num3);
							Application.DoEvents();
							text8 = streamReader2.ReadLine();
							fileName = "table_occult.txt";
							if (http.DownloadHTTP(Settings.Default.LuckyStar + text2 + "/" + text8, fileName, text5, unzip: false, gunzip: false, ShowMessages: false, ShowProgressbar: false))
							{
								DateTime lastWriteTime = new FileInfo(text5).LastWriteTime;
								double num4 = Utilities.JD_from_Date(lastWriteTime.Year, lastWriteTime.Month, (double)lastWriteTime.Day + (double)lastWriteTime.Hour / 24.0) - 2400000.5;
								streamWriter.WriteLine(string.Format("Prediction Date: {0,1:f2}", num4));
								streamWriter.Write(File.ReadAllText(text5));
							}
							num3++;
						}
						while (!streamReader2.EndOfStream);
					}
					((Form)pBar).Close();
				}
				if (File.Exists(text5))
				{
					File.Delete(text5);
				}
			}
			OccultationElements.SiteNeedsUpdating = (OccultationElements.RegionNeedsUpdating = true);
			DisplayMPOccultations.OccElements.Clear();
			Read_LuckyStar_RIO_File(text6, IsUpdateFile: false, 0, "LuckyStar:");
			OccultationElements.SortField = 0;
			DisplayMPOccultations.OccElements.Sort();
			using (StreamWriter streamWriter2 = new StreamWriter(text7))
			{
				streamWriter2.WriteLine("<Occultations>");
				for (int i = 0; i < DisplayMPOccultations.OccElements.Count; i++)
				{
					streamWriter2.Write(DisplayMPOccultations.OccElements[i].XML_Elements());
				}
				streamWriter2.WriteLine("</Occultations>");
			}
			if (NOMADpresent)
			{
				NOMAD.ReleaseNOMAD();
			}
			DisplayMPOccultations.ShowListAndDisplay();
			DisplayMPOccultations.ListAndDisplay.SetSite();
			DisplayMPOccultations.ListAndDisplay.SetSiteTests();
			DisplayMPOccultations.ListAndDisplay.SortAndDisplay(0);
			DisplayMPOccultations.ListAndDisplay.SetDisplayParameters(text7);
			((Control)DisplayMPOccultations.ListAndDisplay).Focus();
		}
	}
}

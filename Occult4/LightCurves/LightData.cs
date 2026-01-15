using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Windows.Forms;
using Occult;

namespace LightCurves
{
	public class LightData
	{
		private static LightCurveData LCD;

		internal static List<LightCurveData> MainLightCurves = new List<LightCurveData>();

		internal static List<LightCurveData> LightCurvesSubmitted = new List<LightCurveData>();

		internal static List<LightCurveData> MyReportedLightCurves = new List<LightCurveData>();

		internal static List<LightCurveData> MyPendingLightCurves = new List<LightCurveData>();

		internal static List<LightCurveData> ToDeleteLightCurves = new List<LightCurveData>();

		internal static List<LightCurveData> ToReviewLightCurves = new List<LightCurveData>();

		internal static LightCurve_MultiPlot MultiPlot;

		internal static PossibleDuplicateFiles PossibleDuplicates;

		public static LightCurve LightCurveForm;

		public static LightCurveViewer LightCurveView;

		internal static AsteroidLC_DataInput AsteroidDataInput;

		internal static Embargo EmbargoForm;

		internal static Remove_Review_status RemoveReview;

		internal static Delete_del_init DeleteDelInit;

		private static LightCurveStarIndex LCSI;

		internal static List<LightCurveStarIndex> HipIndex;

		internal static List<LightCurveStarIndex> SAOIndex;

		internal static List<LightCurveStarIndex> XZ80Index;

		internal static List<LightCurveStarIndex> ZCIndex;

		internal static List<LightCurveStarIndex> Tyc2Index;

		internal static List<LightCurveStarIndex> U4Index;

		internal static List<LightCurveStarIndex> AsteroidIndex;

		internal static List<LightCurveStarIndex> ObserverNameIndex;

		internal static List<LightCurveStarIndex> AsteroidNameIndex;

		internal static List<LightCurveStarIndex> AsteroidNumberIndex;

		internal static List<int> MyRecordNumbers;

		internal static List<int> SubmittedRecordNumbers;

		internal static List<int> PendingRecordNumbers;

		internal static List<int> ToDeleteRecordNumbers;

		internal static List<int> ToReviewRecordNumbers;

		internal static bool IndexesCreated = false;

		internal static int LightCurveIndex_Current = 0;

		internal static float HeightRatioForSelectedLightLevel = 0f;

		internal static float YatTopOfPlot = 0f;

		internal static float YatBottomOfPlot = 0f;

		internal static string HeaderExtraText = "";

		internal static bool AddExtraHeader;

		internal static double HeaderExtraTime_UT1 = 0.0;

		internal static double HeaderExtraTime_UT2 = 0.0;

		internal static bool AddTimeMarker1 = false;

		internal static bool AddTimeMarker2 = false;

		private ArrayList Indices = new ArrayList();

		private ArrayList Dates = new ArrayList();

		internal static double CurrentPlotDuration = 10.0;

		internal static float Interval = 0.1f;

		internal static float ZeroHeight;

		internal static double CurrentPlotStartTime_Secs = 100.0;

		public static void ShowLightCurveForm()
		{
			try
			{
				((Control)LightCurveForm).Show();
			}
			catch
			{
				LightCurveForm = new LightCurve();
				((Control)LightCurveForm).Show();
			}
		}

		public static void ShowLightCurveViewer()
		{
			try
			{
				((Control)LightCurveView).Show();
			}
			catch
			{
				LightCurveView = new LightCurveViewer();
				((Control)LightCurveView).Show();
			}
		}

		public static void ShowPossibleDuplicates()
		{
			try
			{
				((Control)PossibleDuplicates).Show();
			}
			catch
			{
				PossibleDuplicates = new PossibleDuplicateFiles();
				((Control)PossibleDuplicates).Show();
			}
		}

		public static void ShowAsteroidLCdataInput()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)AsteroidDataInput).ShowDialog();
			}
			catch
			{
				AsteroidDataInput = new AsteroidLC_DataInput();
				((Form)AsteroidDataInput).ShowDialog();
			}
		}

		public static void ShowRemoveReview()
		{
			try
			{
				((Control)RemoveReview).Show();
			}
			catch
			{
				RemoveReview = new Remove_Review_status();
				((Control)RemoveReview).Show();
			}
		}

		public static void ShowDeleteDelInit()
		{
			try
			{
				((Control)DeleteDelInit).Show();
			}
			catch
			{
				DeleteDelInit = new Delete_del_init();
				((Control)DeleteDelInit).Show();
			}
		}

		internal static void ReadMainFile(bool Index, bool ReRead, bool ShowReadErrorMessages)
		{
			//IL_0162: Unknown result type (might be due to invalid IL or missing references)
			if (!ReRead && MainLightCurves.Count > 1000)
			{
				return;
			}
			MainLightCurves = new List<LightCurveData>();
			HipIndex = new List<LightCurveStarIndex>();
			SAOIndex = new List<LightCurveStarIndex>();
			XZ80Index = new List<LightCurveStarIndex>();
			ZCIndex = new List<LightCurveStarIndex>();
			Tyc2Index = new List<LightCurveStarIndex>();
			U4Index = new List<LightCurveStarIndex>();
			AsteroidIndex = new List<LightCurveStarIndex>();
			AsteroidNameIndex = new List<LightCurveStarIndex>();
			ObserverNameIndex = new List<LightCurveStarIndex>();
			AsteroidNumberIndex = new List<LightCurveStarIndex>();
			MainLightCurves.Clear();
			string[] array = new string[6];
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\LightCurves\\LightCurves.txt", Encoding.ASCII))
			{
				do
				{
					array[0] = (array[1] = (array[2] = (array[3] = (array[4] = (array[5] = "")))));
					for (int i = 0; i < 6; i++)
					{
						array[i] = streamReader.ReadLine();
					}
					LCD = new LightCurveData();
					if (LCD.ReadObservation(array, out var ErrorLine))
					{
						LCD.FileName = "";
						LCD.ExcludeLightCurve = false;
						MainLightCurves.Add(LCD);
					}
					else if (ShowReadErrorMessages)
					{
						MessageBox.Show("Error reading LightCurve #" + (MainLightCurves.Count + 1) + "\r\n\r\n" + ErrorLine, "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				while (!streamReader.EndOfStream);
			}
			if (Index)
			{
				SortMainLightCurves_andIndex();
			}
		}

		internal static void SortMainLightCurves_andIndex()
		{
			IndexesCreated = false;
			MainLightCurves.Sort();
			HipIndex.Clear();
			SAOIndex.Clear();
			XZ80Index.Clear();
			ZCIndex.Clear();
			U4Index.Clear();
			Tyc2Index.Clear();
			AsteroidIndex.Clear();
			AsteroidNameIndex.Clear();
			ObserverNameIndex.Clear();
			for (int i = 0; i < MainLightCurves.Count; i++)
			{
				if (MainLightCurves[i].Hipparcos > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].Hipparcos, 0, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 0;
					HipIndex.Add(LCSI);
				}
				if (MainLightCurves[i].SAO > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].SAO, 0, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 1;
					SAOIndex.Add(LCSI);
				}
				if (MainLightCurves[i].XZ > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].XZ, 0, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 2;
					XZ80Index.Add(LCSI);
				}
				if (MainLightCurves[i].ZC > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].ZC, 0, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 3;
					ZCIndex.Add(LCSI);
				}
				if (MainLightCurves[i].Tyc2A > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].Tyc2A, MainLightCurves[i].Tyc2B, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 4;
					Tyc2Index.Add(LCSI);
				}
				if (MainLightCurves[i].U4Zone > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].U4Zone, MainLightCurves[i].U4Number, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 5;
					U4Index.Add(LCSI);
				}
				if (MainLightCurves[i].AsteroidNumber > 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetValues(i, MainLightCurves[i].AsteroidNumber, 0, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.Observer = MainLightCurves[i].Observer;
					LCSI.CatalogueNumber = 6;
					AsteroidIndex.Add(LCSI);
				}
			}
			HipIndex.Sort();
			SAOIndex.Sort();
			XZ80Index.Sort();
			ZCIndex.Sort();
			U4Index.Sort();
			Tyc2Index.Sort();
			AsteroidIndex.Sort();
			IndexesCreated = true;
		}

		internal static void CreateObserverNameIndex(string NameChars)
		{
			ObserverNameIndex.Clear();
			string value = NameChars.ToLower();
			for (int i = 0; i < MainLightCurves.Count; i++)
			{
				if (MainLightCurves[i].Observer.ToLower().Contains(value))
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetObserverNames(i, MainLightCurves[i].Observer, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 7;
					ObserverNameIndex.Add(LCSI);
				}
			}
		}

		internal static void CreateObserverEventIndex(string NameChars)
		{
			string text = "";
			string value = NameChars.ToLower();
			for (int i = 0; i < MainLightCurves.Count; i++)
			{
				if (MainLightCurves[i].Observer.ToLower().Contains(value))
				{
					string text2 = MainLightCurves[i].Observer.PadRight(15);
					string text3 = MainLightCurves[i].EventDate.PadRight(12);
					string text4 = ("(" + MainLightCurves[i].AsteroidNumber + ")").PadLeft(8) + " " + MainLightCurves[i].AsteroidName;
					string text5 = " XZ" + MainLightCurves[i].XZ.ToString().PadLeft(6);
					text = ((MainLightCurves[i].AsteroidNumber <= 0) ? (text + text2 + text3 + text5 + "\r\n") : (text + text2 + text3 + text4 + "\r\n"));
				}
			}
			Clipboard.SetText(text);
		}

		internal static void CreateAsteroidNameIndex(string NameChars)
		{
			AsteroidNameIndex.Clear();
			NameChars.ToLower();
			int length = NameChars.Length;
			for (int i = 0; i < MainLightCurves.Count; i++)
			{
				if (MainLightCurves[i].AsteroidName.Length >= length && MainLightCurves[i].AsteroidName.ToLower().IndexOf(NameChars) == 0)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetAsteroidNames(i, MainLightCurves[i].AsteroidName, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 8;
					AsteroidNameIndex.Add(LCSI);
				}
			}
			LightCurveStarIndex.SortByName = true;
			LightCurveStarIndex.SortByDate = false;
			AsteroidNameIndex.Sort();
		}

		internal static void CreateAsteroidNumberIndex(int AstNumber)
		{
			AsteroidNumberIndex.Clear();
			for (int i = 0; i < MainLightCurves.Count; i++)
			{
				if (MainLightCurves[i].AsteroidNumber == AstNumber)
				{
					LCSI = new LightCurveStarIndex();
					LCSI.SetAsteroidNumbers(i, MainLightCurves[i].AsteroidNumber, MainLightCurves[i].EventDate, MainLightCurves[i].JD);
					LCSI.CatalogueNumber = 6;
					AsteroidNumberIndex.Add(LCSI);
				}
			}
			LightCurveStarIndex.SortByName = false;
			LightCurveStarIndex.SortByDate = false;
			AsteroidNumberIndex.Sort();
		}

		internal static void MainLightCurve_EntriesForAnObject(ref List<int> LightCurveRecordNumbers, ref int NumLightCurves, int AsteroidNumber, string AsteroidName, double JD_EventDate, bool DateToWithin4Hours)
		{
			int num = 0;
			int num2 = AsteroidIndex.Count - 1;
			int num3 = 0;
			int num4 = 0;
			bool flag = false;
			NumLightCurves = 0;
			double num5 = 1.0;
			if (DateToWithin4Hours)
			{
				num5 = 0.17;
			}
			if (AsteroidNumber > 0)
			{
				do
				{
					num3 = (num2 + num) / 2;
					if (AsteroidIndex[num3].StarNum1_or_AsteroidNo == AsteroidNumber)
					{
						flag = true;
						num4 = num3;
						do
						{
							num4 -= 4;
							if (num4 < 0)
							{
								num4 = 0;
							}
						}
						while (AsteroidIndex[num4].StarNum1_or_AsteroidNo == AsteroidNumber && num4 > 0);
						break;
					}
					if (AsteroidIndex[num3].StarNum1_or_AsteroidNo > AsteroidNumber)
					{
						num2 = num3 - 1;
					}
					else
					{
						num = num3 + 1;
					}
				}
				while (num <= num2);
				if (!flag)
				{
					return;
				}
				if (num4 < 0)
				{
					num4 = 0;
				}
				for (int i = num4; i < AsteroidIndex.Count; i++)
				{
					if (AsteroidIndex[i].StarNum1_or_AsteroidNo >= AsteroidNumber)
					{
						if (AsteroidIndex[i].StarNum1_or_AsteroidNo > AsteroidNumber)
						{
							break;
						}
						if (Math.Abs(AsteroidIndex[i].JD - JD_EventDate) < num5)
						{
							LightCurveRecordNumbers.Add(AsteroidIndex[i].LightCurveIndex);
							NumLightCurves++;
						}
					}
				}
			}
			else
			{
				if (AsteroidName.Length <= 4)
				{
					return;
				}
				for (int j = num4; j < MainLightCurves.Count; j++)
				{
					if (MainLightCurves[j].AsteroidName.ToLower() == AsteroidName)
					{
						LightCurveRecordNumbers.Add(j);
						NumLightCurves++;
					}
				}
			}
		}

		internal static void SubmittedLightCurve_EntriesForAnObject(ref List<int> SubmittedEntries, int AsteroidNumber, string AsteroidName, double JD_EventDate, bool DateToWithin4Hours)
		{
			SubmittedEntries.Clear();
			double num = 1.0;
			if (DateToWithin4Hours)
			{
				num = 0.17;
			}
			for (int i = 0; i < LightCurvesSubmitted.Count; i++)
			{
				if (!(JD_EventDate > 2000000.0) || !(Math.Abs(AsteroidIndex[i].JD - JD_EventDate) < num))
				{
					if (LightCurvesSubmitted[i].AsteroidNumber == AsteroidNumber)
					{
						SubmittedEntries.Add(i);
					}
					else if (AsteroidName.Length > 4 && LightCurvesSubmitted[i].AsteroidName.ToLower() == AsteroidName)
					{
						SubmittedEntries.Add(i);
					}
				}
			}
		}

		internal static void SaveMainLightCurves(bool RemoveExcluded)
		{
			string text = Utilities.AppPath + "\\LightCurves\\LightCurves.txt";
			string fileNameWithoutExtension = Path.GetFileNameWithoutExtension("LightCurves.txt");
			string text2 = "_" + DateTime.Now.Year + DateTime.Now.Month.ToString().PadLeft(2, '0') + DateTime.Now.Day.ToString().PadLeft(2, '0');
			int num = 1;
			for (int i = 1; i < 50; i++)
			{
				string text3 = Utilities.AppPath + "\\LightCurves\\" + fileNameWithoutExtension + text2 + "_" + num + ".txt";
				if (!File.Exists(text3))
				{
					File.Move(text, text3);
					break;
				}
				num++;
			}
			using (StreamWriter streamWriter = new StreamWriter(text))
			{
				for (int j = 0; j < MainLightCurves.Count; j++)
				{
					if (!(MainLightCurves[j].ExcludeLightCurve && RemoveExcluded))
					{
						streamWriter.WriteLine(MainLightCurves[j].ToString());
					}
				}
			}
			ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
		}

		internal static void LoadLightCurvesReceived_Admin(bool IncludeEmbargoed, bool includeForReview, int AsteroidNumber, bool ShowReadErrorMessages)
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Invalid comparison between Unknown and I4
			//IL_00ae: Unknown result type (might be due to invalid IL or missing references)
			if (!Directory.Exists(Utilities.AppPath + "\\LightCurveReports"))
			{
				if ((int)MessageBox.Show("The directory for containing submitted light curves does not exist.\r\n\r\nDo you want to create that directory?", "No light curves", (MessageBoxButtons)4, (MessageBoxIcon)16) == 7)
				{
					return;
				}
				Directory.CreateDirectory(Utilities.AppPath + "\\LightCurveReports");
			}
			LightCurvesSubmitted = new List<LightCurveData>();
			string[] lines = new string[6];
			int recordNumber = 0;
			SubmittedRecordNumbers = new List<int>();
			EmbargoOps.ReadEmbargoes();
			EmbargoOps.SetClearEmbargoStatus();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.dat");
			foreach (string text in files)
			{
				try
				{
					recordNumber = ReadSubmittedFiles(lines, recordNumber, text, AsteroidNumber, ShowReadErrorMessages);
				}
				catch
				{
					MessageBox.Show("Error reading \r\n" + text, "ReadError", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
			}
			if (IncludeEmbargoed)
			{
				files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.embargoed");
				foreach (string f in files)
				{
					recordNumber = ReadSubmittedFiles(lines, recordNumber, f, AsteroidNumber, ShowReadErrorMessages: true);
				}
			}
			if (IncludeEmbargoed)
			{
				files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.review");
				foreach (string f2 in files)
				{
					recordNumber = ReadSubmittedFiles(lines, recordNumber, f2, AsteroidNumber, ShowReadErrorMessages: true);
				}
			}
		}

		private static int ReadSubmittedFiles(string[] Lines, int RecordNumber, string F, int AsteroidNumber, bool ShowReadErrorMessages)
		{
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string[] array = new string[5] { "Date:", "Star:", "Observer:", "Object:", "Values:" };
			using StreamReader streamReader = new StreamReader(F, Encoding.ASCII);
			do
			{
				text = "";
				string[] array2 = streamReader.ReadToEnd().Replace("\r", "").Split(new char[1] { '\n' });
				for (int i = 0; i < 5; i++)
				{
					Lines[i] = array2[i];
				}
				for (int j = 5; j < array2.Length; j++)
				{
					Lines[4] += array2[j];
				}
				for (int k = 0; k < 5; k++)
				{
					if (!Lines[k].Contains(array[k]))
					{
						text = text + k + ", ";
					}
				}
				if (text.Length > 0)
				{
					if (ShowReadErrorMessages)
					{
						MessageBox.Show("Error reading Submitted LightCurve file:\r\n\r\n" + F + "\r\n\r\n at lines " + text + "\r\nThis file will require manual editing", "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
						return RecordNumber;
					}
					return RecordNumber;
				}
				LCD = new LightCurveData();
				if (LCD.ReadObservation(Lines, out var ErrorLine))
				{
					LCD.FileName = Path.GetFileName(F);
					LCD.ExcludeLightCurve = false;
					LCD.ReviewLightCurve = LCD.FileName.Contains(".review");
					LightCurvesSubmitted.Add(LCD);
					if ((AsteroidNumber == 0) | (AsteroidNumber == LCD.AsteroidNumber))
					{
						SubmittedRecordNumbers.Add(RecordNumber);
					}
					RecordNumber++;
				}
				else if (ShowReadErrorMessages)
				{
					MessageBox.Show("Error reading Submitted LightCurve file:\r\n\r\n" + F + "\r\n\r\n" + ErrorLine + "\r\nThis file will require manual editing", "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				}
			}
			while (!streamReader.EndOfStream);
			return RecordNumber;
		}

		internal static void LoadToDeleteLightCurves()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			if (!Directory.Exists(Utilities.AppPath + "\\LightCurveReports"))
			{
				MessageBox.Show("The directory for containing submitted light curves does not exist", "No light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			ToDeleteLightCurves = new List<LightCurveData>();
			string[] array = new string[6];
			int num = 0;
			ToDeleteRecordNumbers = new List<int>();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.del");
			for (int i = 0; i <= 1; i++)
			{
				if (i == 1)
				{
					files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.init");
				}
				string[] array2 = files;
				foreach (string text in array2)
				{
					using StreamReader streamReader = new StreamReader(text, Encoding.ASCII);
					do
					{
						for (int k = 0; k < 6; k++)
						{
							array[k] = streamReader.ReadLine();
						}
						LCD = new LightCurveData();
						if (LCD.ReadObservation(array, out var ErrorLine))
						{
							LCD.FileName = Path.GetFileName(text);
							LCD.ExcludeLightCurve = false;
							ToDeleteLightCurves.Add(LCD);
							ToDeleteRecordNumbers.Add(num);
							num++;
						}
						else
						{
							MessageBox.Show("Error reading ToDelete LightCurve file:\r\n" + text + "\r\n\r\n" + ErrorLine + "\r\n\r\nThis file will require manual editing", "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
						}
					}
					while (!streamReader.EndOfStream);
				}
			}
		}

		internal static void LoadToReviewLightCurves()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0120: Unknown result type (might be due to invalid IL or missing references)
			if (!Directory.Exists(Utilities.AppPath + "\\LightCurveReports"))
			{
				MessageBox.Show("The directory for containing submitted light curves does not exist", "No light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			ToReviewLightCurves = new List<LightCurveData>();
			string[] array = new string[6];
			int num = 0;
			ToReviewRecordNumbers = new List<int>();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\LightCurveReports", "*.review");
			foreach (string text in files)
			{
				using StreamReader streamReader = new StreamReader(text, Encoding.ASCII);
				do
				{
					for (int j = 0; j < 6; j++)
					{
						array[j] = streamReader.ReadLine();
					}
					LCD = new LightCurveData();
					if (LCD.ReadObservation(array, out var ErrorLine))
					{
						LCD.FileName = Path.GetFileName(text);
						LCD.ReviewLightCurve = true;
						ToReviewLightCurves.Add(LCD);
						ToReviewRecordNumbers.Add(num);
						num++;
					}
					else
					{
						MessageBox.Show("Error reading ToReview LightCurve file:\r\n" + text + "\r\n\r\n" + ErrorLine + "\r\n\r\nThis file will require manual editing", "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				while (!streamReader.EndOfStream);
			}
		}

		internal static void MergeMainAndSubmitted()
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0070: Unknown result type (might be due to invalid IL or missing references)
			//IL_015e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0164: Invalid comparison between Unknown and I4
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			if ((int)MessageBox.Show("This function will:\r\n*  load the main file, and all submitted files\r\n*  merge the submitted files into the main file\r\n*  save the updated main file, keeping the old file under\r\n     a different name\r\n*  zip all the submitted files into a different directory, and \r\n*  delete the individual submitted files.\r\n\r\nYou should make sure that you have made any edits to\r\n  the submitted files that might be required\r\n\r\n   Do you want to proceed?", "Confirm merge", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.cmdCancel).set_Visible(false);
			((Control)messageForm).Show();
			((Control)messageForm.label).set_Text("Applying embargos");
			Application.DoEvents();
			LoadLightCurvesReceived_Admin(IncludeEmbargoed: false, includeForReview: false, 0, ShowReadErrorMessages: true);
			if (LightCurvesSubmitted.Count < 1)
			{
				MessageBox.Show("There are no submitted light curves to be merged", "No submitted light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Form)messageForm).Close();
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			((Control)messageForm.label).set_Text("Reading main file of light curves");
			Application.DoEvents();
			ReadMainFile(Index: false, ReRead: true, ShowReadErrorMessages: true);
			((Control)messageForm.label).set_Text("Reading & Adding submitted light curves");
			Application.DoEvents();
			for (int i = 0; i < LightCurvesSubmitted.Count; i++)
			{
				if (!LightCurvesSubmitted[i].ReviewLightCurve)
				{
					LCD = new LightCurveData();
					LCD = LightCurvesSubmitted[i];
					MainLightCurves.Add(LCD);
				}
			}
			((Control)messageForm.label).set_Text("Sorting the light curves by date");
			Application.DoEvents();
			MainLightCurves.Sort();
			((Control)LightCurveView).set_Cursor(Cursors.get_Default());
			Cursor.set_Current(Cursors.get_Default());
			if ((int)MessageBox.Show("The light curves have been merged.\r\n\r\nThe merged file will now be saved,\r\nand the submitted files will be zipped.\r\n\r\nPlease confirm you want to proceed", "Confirm save", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			((Control)messageForm.label).set_Text("Saving updated light curves");
			Application.DoEvents();
			SaveMainLightCurves(RemoveExcluded: false);
			((Control)messageForm.label).set_Text("Tidying up submitted light curves");
			Application.DoEvents();
			Cursor.set_Current(Cursors.get_WaitCursor());
			ArrayList arrayList = new ArrayList();
			for (int j = 0; j < LightCurvesSubmitted.Count; j++)
			{
				if (!LightCurvesSubmitted[j].ReviewLightCurve)
				{
					arrayList.Add(LightCurvesSubmitted[j].FileName);
				}
			}
			arrayList.Sort();
			string text = Utilities.AppPath + "\\LightCurveReports\\";
			string text2 = "Consolidation_" + DateTime.Now.Year + "_#";
			string path = "";
			string text3 = ".txt";
			int num = 999;
			FileInfo[] files = new DirectoryInfo(text).GetFiles(text2 + "*.txt");
			if (files.Length >= 0)
			{
				FileInfo[] array = files;
				for (int k = 0; k < array.Length; k++)
				{
					string name = array[k].Name;
					int num2 = name.IndexOf("#");
					int num3 = name.IndexOf(".", num2);
					int.TryParse(name.Substring(num2 + 1, num3 - num2 - 1), out var result);
					result += 1000;
					if (result > num)
					{
						num = result;
					}
				}
				num++;
				path = text + text2 + num.ToString().Substring(1) + text3;
			}
			using (StreamWriter streamWriter = new StreamWriter(path))
			{
				for (int l = 0; l < LightCurvesSubmitted.Count; l++)
				{
					if (!LightCurvesSubmitted[l].ReviewLightCurve)
					{
						streamWriter.WriteLine(LightCurvesSubmitted[l].ToString());
					}
				}
			}
			string text4 = Utilities.AppPath + "\\LightCurveReports\\Zipped Source Files";
			if (!Directory.Exists(text4))
			{
				Directory.CreateDirectory(text4);
			}
			using (ZipArchive destination = ZipFile.Open(text4 + "\\" + text2 + num.ToString().Substring(1) + ".zip", ZipArchiveMode.Create))
			{
				foreach (string item in arrayList.ToArray(typeof(string)))
				{
					destination.CreateEntryFromFile(Utilities.AppPath + "\\LightCurveReports\\" + item, new FileInfo(item).Name);
				}
			}
			for (int m = 0; m < arrayList.Count; m++)
			{
				File.Delete(Utilities.AppPath + "\\LightCurveReports\\" + arrayList[m]!.ToString());
			}
			((Control)messageForm.label).set_Text("Reload all light curves");
			Application.DoEvents();
			LightCurveView.ReadLightCurves();
			Cursor.set_Current(Cursors.get_Default());
			((Form)messageForm).Close();
			MessageBox.Show("The submitted light curves have now been processed.\r\n\r\nPlease review the content of the Occult 4\\LightCurveReports\r\nfor the presence of files that should be manually deleted,\r\nincluding any light curve file with the extension 'init' -\r\nwhich is applied to the initially submitted light curve \r\nfile when it is edited.", "No submitted light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
		}

		internal static void LoadMyReports()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d7: Unknown result type (might be due to invalid IL or missing references)
			if (!Directory.Exists(Utilities.AppPath + "\\Observations\\LightCurves\\Reported"))
			{
				MessageBox.Show("You have no reported light curves to show", "No light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			MyReportedLightCurves = new List<LightCurveData>();
			string[] array = new string[6];
			string text = "";
			bool flag = false;
			int num = 0;
			int num2 = 0;
			MyRecordNumbers = new List<int>();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Observations\\LightCurves\\Reported", "*.dat");
			foreach (string text2 in files)
			{
				flag = false;
				using (StreamReader streamReader = new StreamReader(text2, Encoding.ASCII))
				{
					do
					{
						try
						{
							array = streamReader.ReadToEnd().Replace("\r", string.Empty).Split(new char[1] { '\n' });
							if ((array[0].Substring(0, 1) == "#") | (array.Length < 6))
							{
								flag = true;
								continue;
							}
						}
						catch
						{
							continue;
						}
						LCD = new LightCurveData();
						if (LCD.ReadObservation(array, out var ErrorLine))
						{
							LCD.FileName = Path.GetFileName(text2);
							LCD.ExcludeLightCurve = true;
							MyReportedLightCurves.Add(LCD);
							MyRecordNumbers.Add(num);
							num++;
						}
						else
						{
							MessageBox.Show("Error reading My LightCurve file:\r\n" + text2 + "\r\n\r\n" + ErrorLine, "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
						}
					}
					while (!streamReader.EndOfStream);
				}
				if (flag)
				{
					num2++;
					string destFileName = text2.Replace(".dat", ".dat0");
					File.Move(text2, destFileName);
					text = text + Path.GetFileName(text2) + "\r\n";
				}
			}
			if (num2 > 0)
			{
				MessageBox.Show("The following " + num2 + " files were in the 'old' format. They have not been read, and have had their extension changed to '.dat0'\r\n\r\n" + text, "Past submitted light curves in the OLD format", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		internal static void LoadMyPendingReports()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_012c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0179: Unknown result type (might be due to invalid IL or missing references)
			if (!Directory.Exists(Utilities.AppPath + "\\Observations\\LightCurves"))
			{
				MessageBox.Show("You have no pending light curves to show", "No light curves", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			MyPendingLightCurves = new List<LightCurveData>();
			string[] array = new string[6];
			int num = 0;
			int num2 = 0;
			PendingRecordNumbers = new List<int>();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Observations\\LightCurves\\", "*.dat");
			foreach (string text in files)
			{
				using StreamReader streamReader = new StreamReader(text, Encoding.ASCII);
				do
				{
					try
					{
						for (int j = 0; j < 6; j++)
						{
							array[j] = streamReader.ReadLine();
						}
						if (array[0].Substring(0, 1) == "#")
						{
							num2++;
							continue;
						}
					}
					catch
					{
						continue;
					}
					LCD = new LightCurveData();
					if (LCD.ReadObservation(array, out var ErrorLine))
					{
						LCD.FileName = Path.GetFileName(text);
						LCD.ExcludeLightCurve = true;
						MyPendingLightCurves.Add(LCD);
						PendingRecordNumbers.Add(num);
						num++;
					}
					else
					{
						MessageBox.Show("Error reading my pending LightCurve file:\r\n" + text + "\r\n\r\n" + ErrorLine, "Reading Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					}
				}
				while (!streamReader.EndOfStream);
			}
			if (num2 > 0)
			{
				MessageBox.Show(num2 + " light curves in the 'old' format have not been read", "Old format light curves", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		internal static int HipCount(int HipNumber)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < HipIndex.Count; i++)
			{
				if (HipIndex[i].StarNum1_or_AsteroidNo == HipNumber)
				{
					num++;
				}
			}
			return num;
		}

		internal static int SAOCount(int SAONumber)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < SAOIndex.Count; i++)
			{
				if (SAOIndex[i].StarNum1_or_AsteroidNo == SAONumber)
				{
					num++;
				}
			}
			return num;
		}

		internal static int XZ80Count(int XZ80Number)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < XZ80Index.Count; i++)
			{
				if (XZ80Index[i].StarNum1_or_AsteroidNo == XZ80Number)
				{
					num++;
				}
			}
			return num;
		}

		internal static int ZCCount(int ZCNumber)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < ZCIndex.Count; i++)
			{
				if (ZCIndex[i].StarNum1_or_AsteroidNo == ZCNumber)
				{
					num++;
				}
			}
			return num;
		}

		internal static int Tyc2Count(int T2Zone, int T2Number)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < Tyc2Index.Count; i++)
			{
				if (Tyc2Index[i].StarNum1_or_AsteroidNo == T2Zone)
				{
					if (Tyc2Index[i].StarNum2 == T2Number)
					{
						num++;
					}
				}
				else if (Tyc2Index[i].StarNum1_or_AsteroidNo > T2Zone)
				{
					break;
				}
			}
			return num;
		}

		internal static int U4Count(int U4zone, int U4Number)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < U4Index.Count; i++)
			{
				if (U4Index[i].StarNum1_or_AsteroidNo == U4zone)
				{
					if (U4Index[i].StarNum2 == U4Number)
					{
						num++;
					}
				}
				else if (U4Index[i].StarNum1_or_AsteroidNo > U4zone)
				{
					break;
				}
			}
			return num;
		}

		internal static int AsteroidCount(int AsteroidNumber)
		{
			if (MainLightCurves.Count < 1)
			{
				ReadMainFile(Index: true, ReRead: true, ShowReadErrorMessages: true);
			}
			int num = 0;
			for (int i = 0; i < AsteroidIndex.Count; i++)
			{
				if (AsteroidIndex[i].StarNum1_or_AsteroidNo == AsteroidNumber)
				{
					num++;
				}
			}
			return num;
		}

		internal static Bitmap LightCurveImage(int RecordNumber, int PlotWidth, int PlotHeight, int PlotTopHeight, float PlotScale, float VerticalScale, bool Main, bool Submitted, bool Mine, bool Pending, bool ToDelete, bool ToReview, bool ShowSkippedFrames, bool IsMultiPlot, bool ThickLines)
		{
			string Header;
			return LightCurveImage(RecordNumber, PlotWidth, PlotHeight, PlotTopHeight, PlotScale, VerticalScale, Main, Submitted, Mine, Pending, ToDelete, ToReview, ShowSkippedFrames, -1, -1, -1, -1, Truncate: false, -1, IsMultiPlot, ThickLines, out Header);
		}

		internal static Bitmap LightCurveImage(int RecordNumber, int PlotWidth, int PlotHeight, int PlotTopHeight, float PlotScale, float VerticalScale, bool Main, bool Submitted, bool Mine, bool Pending, bool ToDelete, bool ToReview, bool ShowSkippedFrames, bool IsMultiPlot, bool ThickLines, out string Header)
		{
			return LightCurveImage(RecordNumber, PlotWidth, PlotHeight, PlotTopHeight, PlotScale, VerticalScale, Main, Submitted, Mine, Pending, ToDelete, ToReview, ShowSkippedFrames, -1, -1, -1, -1, Truncate: false, -1, IsMultiPlot, ThickLines, out Header);
		}

		internal static Bitmap LightCurveImage(int RecordNumber, int PlotWidth, int PlotHeight, int PlotTopHeight, float PlotScale, float VerticalPlotScale, bool Main, bool Submitted, bool Mine, bool Pending, bool ToDelete, bool ToReview, bool ShowSkippedFrames, int StartHighlightFrame, int EndHighlightFrame, int FirstIntegration_TruncationFrame, int FramesToCombine, bool Truncate, int LastTruncationFrame, bool IsMultiplot, bool ThickLines, out string Header)
		{
			float num = 10000f;
			float num2 = -10000f;
			float num3 = 0f;
			float num4 = 0f;
			float num5 = 0f;
			float num6 = 1f;
			int num7 = 1;
			float num8 = 100f;
			int num9 = 0;
			int num10 = 0;
			new Font("Arial", 8f, FontStyle.Bold);
			_ = Brushes.Black;
			Pen pen = new Pen(Color.Black, 1f);
			new Pen(Color.FromArgb(100, Color.Plum));
			Brush lemonChiffon = Brushes.LemonChiffon;
			Brush brush = new SolidBrush(Color.FromArgb(200, 240, 248, 255));
			Brush brush2 = new SolidBrush(Color.FromArgb(50, 240, 248, 255));
			new Pen(Color.Orange, 1f);
			Pen pen2 = new Pen(Color.Red, 1.5f);
			Brush darkBlue = Brushes.DarkBlue;
			Pen pen3 = new Pen(Color.Brown, 2f);
			Pen[] array = new Pen[2]
			{
				new Pen(Color.Red, 2f),
				new Pen(Color.DarkViolet, 2f)
			};
			float width = 1f;
			if (ThickLines)
			{
				width = 2f;
			}
			Pen pen4 = new Pen(Color.FromArgb(130, Color.DarkBlue), width);
			_ = Brushes.DarkCyan;
			Pen pen5 = new Pen(Color.FromArgb(35, 255, 0, 0), 1f);
			new Pen(Color.Maroon, 1.5f);
			Pen pen6 = new Pen(Color.Red, 1f);
			Pen pen7 = new Pen(Color.Green, 1f);
			float[] array3 = (pen7.DashPattern = new float[2] { 1f, 4f });
			Brush antiqueWhite = Brushes.AntiqueWhite;
			int num11 = 1;
			bool flag = false;
			if (Main)
			{
				num11 = MainLightCurves[RecordNumber].NumPoints - 1;
			}
			else if (Mine)
			{
				num11 = MyReportedLightCurves[RecordNumber].NumPoints - 1;
			}
			else if (Submitted)
			{
				num11 = LightCurvesSubmitted[RecordNumber].NumPoints - 1;
				flag = LightCurvesSubmitted[RecordNumber].FileName.Contains("embargoed");
			}
			else if (Pending)
			{
				num11 = MyPendingLightCurves[RecordNumber].NumPoints - 1;
			}
			else if (ToDelete)
			{
				num11 = ToDeleteLightCurves[RecordNumber].NumPoints - 1;
			}
			else if (ToReview)
			{
				num11 = ToReviewLightCurves[RecordNumber].NumPoints - 1;
			}
			for (int i = 0; i <= num11; i++)
			{
				if (Main)
				{
					if ((float)MainLightCurves[RecordNumber].LightValues[i] < num)
					{
						num = MainLightCurves[RecordNumber].LightValues[i];
					}
					if ((float)MainLightCurves[RecordNumber].LightValues[i] > num2)
					{
						num2 = MainLightCurves[RecordNumber].LightValues[i];
					}
				}
				else if (Mine)
				{
					if ((float)MyReportedLightCurves[RecordNumber].LightValues[i] < num)
					{
						num = MyReportedLightCurves[RecordNumber].LightValues[i];
					}
					if ((float)MyReportedLightCurves[RecordNumber].LightValues[i] > num2)
					{
						num2 = MyReportedLightCurves[RecordNumber].LightValues[i];
					}
				}
				if (Submitted)
				{
					if ((float)LightCurvesSubmitted[RecordNumber].LightValues[i] < num)
					{
						num = LightCurvesSubmitted[RecordNumber].LightValues[i];
					}
					if ((float)LightCurvesSubmitted[RecordNumber].LightValues[i] > num2)
					{
						num2 = LightCurvesSubmitted[RecordNumber].LightValues[i];
					}
				}
				if (Pending)
				{
					if ((float)MyPendingLightCurves[RecordNumber].LightValues[i] < num)
					{
						num = MyPendingLightCurves[RecordNumber].LightValues[i];
					}
					if ((float)MyPendingLightCurves[RecordNumber].LightValues[i] > num2)
					{
						num2 = MyPendingLightCurves[RecordNumber].LightValues[i];
					}
				}
				if (ToDelete)
				{
					if ((float)ToDeleteLightCurves[RecordNumber].LightValues[i] < num)
					{
						num = ToDeleteLightCurves[RecordNumber].LightValues[i];
					}
					if ((float)ToDeleteLightCurves[RecordNumber].LightValues[i] > num2)
					{
						num2 = ToDeleteLightCurves[RecordNumber].LightValues[i];
					}
				}
				if (ToReview)
				{
					if ((float)ToReviewLightCurves[RecordNumber].LightValues[i] < num)
					{
						num = ToReviewLightCurves[RecordNumber].LightValues[i];
					}
					if ((float)ToReviewLightCurves[RecordNumber].LightValues[i] > num2)
					{
						num2 = ToReviewLightCurves[RecordNumber].LightValues[i];
					}
				}
			}
			if (num >= 0f)
			{
				num = 0f;
			}
			num -= 0.1f * num2;
			num2 *= 1.1f;
			num2 /= VerticalPlotScale;
			float num12;
			if (num2 != num)
			{
				num12 = (float)PlotHeight / (num2 - num);
				ZeroHeight = (0f - num) / (num2 - num) * (float)PlotHeight + 1f;
			}
			else
			{
				num12 = 1f;
				ZeroHeight = (float)PlotTopHeight + (float)PlotHeight * 0.1f;
			}
			YatTopOfPlot = PlotTopHeight;
			YatBottomOfPlot = PlotHeight + PlotTopHeight;
			Bitmap bitmap = new Bitmap(PlotWidth, 2 * PlotTopHeight + PlotHeight + 6);
			Graphics graphics = Graphics.FromImage(bitmap);
			graphics.SmoothingMode = SmoothingMode.AntiAlias;
			if (flag)
			{
				graphics.Clear(Color.Wheat);
			}
			else
			{
				graphics.Clear(Color.White);
			}
			if (StartHighlightFrame >= 0 && EndHighlightFrame >= StartHighlightFrame)
			{
				if (EndHighlightFrame - StartHighlightFrame == 0)
				{
					graphics.DrawLine(pen5, (float)StartHighlightFrame * PlotScale, PlotTopHeight, (float)StartHighlightFrame * PlotScale, PlotHeight);
				}
				else
				{
					graphics.FillRectangle(lemonChiffon, (float)StartHighlightFrame * PlotScale, PlotTopHeight, (float)(EndHighlightFrame - StartHighlightFrame) * PlotScale, PlotHeight);
				}
			}
			float num13;
			if (Main || Submitted)
			{
				num13 = YatBottomOfPlot + HeightRatioForSelectedLightLevel * (YatTopOfPlot - YatBottomOfPlot) * VerticalPlotScale;
				if (!IsMultiplot & (HeightRatioForSelectedLightLevel < 1f))
				{
					graphics.DrawLine(pen2, 0f, num13, PlotWidth, num13);
				}
			}
			if (Truncate)
			{
				if (FirstIntegration_TruncationFrame > 0)
				{
					graphics.FillRectangle(antiqueWhite, 0f, 0f, (float)FirstIntegration_TruncationFrame * PlotScale, PlotHeight);
				}
				if (LastTruncationFrame < num11)
				{
					graphics.FillRectangle(antiqueWhite, (float)LastTruncationFrame * PlotScale, 0f, PlotWidth, PlotHeight);
				}
			}
			StringBuilder stringBuilder = new StringBuilder();
			if (Main)
			{
				if (MainLightCurves[RecordNumber].AsteroidName.Length > 0)
				{
					stringBuilder.Append("(" + MainLightCurves[RecordNumber].AsteroidNumber + ") " + MainLightCurves[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + MainLightCurves[RecordNumber].XZ);
					if (MainLightCurves[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + MainLightCurves[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + MainLightCurves[RecordNumber].EventDate);
				stringBuilder.Append(",  " + MainLightCurves[RecordNumber].Observer.Replace("/", " + "));
			}
			else if (Mine)
			{
				if (MyReportedLightCurves[RecordNumber].AsteroidNumber > 0)
				{
					stringBuilder.Append("(" + MyReportedLightCurves[RecordNumber].AsteroidNumber + ") " + MyReportedLightCurves[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + MyReportedLightCurves[RecordNumber].XZ);
					if (MyReportedLightCurves[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + MyReportedLightCurves[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + MyReportedLightCurves[RecordNumber].EventDate);
				stringBuilder.Append(",  " + MyReportedLightCurves[RecordNumber].Observer.Replace("/", " + "));
			}
			else if (Submitted)
			{
				if (LightCurvesSubmitted[RecordNumber].AsteroidNumber > 0)
				{
					stringBuilder.Append("(" + LightCurvesSubmitted[RecordNumber].AsteroidNumber + ") " + LightCurvesSubmitted[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + LightCurvesSubmitted[RecordNumber].XZ);
					if (LightCurvesSubmitted[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + LightCurvesSubmitted[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + LightCurvesSubmitted[RecordNumber].EventDate);
				stringBuilder.Append(",  " + LightCurvesSubmitted[RecordNumber].Observer.Replace("/", " + "));
			}
			else if (Pending)
			{
				if (MyPendingLightCurves[RecordNumber].AsteroidNumber > 0)
				{
					stringBuilder.Append("(" + MyPendingLightCurves[RecordNumber].AsteroidNumber + ") " + MyPendingLightCurves[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + MyPendingLightCurves[RecordNumber].XZ);
					if (MyPendingLightCurves[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + MyPendingLightCurves[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + MyPendingLightCurves[RecordNumber].EventDate);
				stringBuilder.Append(",  " + MyPendingLightCurves[RecordNumber].Observer.Replace("/", " + "));
			}
			else if (ToDelete)
			{
				if (ToDeleteLightCurves[RecordNumber].IsAsteroid)
				{
					stringBuilder.Append("(" + ToDeleteLightCurves[RecordNumber].AsteroidNumber + ") " + ToDeleteLightCurves[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + ToDeleteLightCurves[RecordNumber].XZ);
					if (ToDeleteLightCurves[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + ToDeleteLightCurves[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + ToDeleteLightCurves[RecordNumber].EventDate);
				stringBuilder.Append(",  " + ToDeleteLightCurves[RecordNumber].Observer.Replace("/", " + "));
			}
			else if (ToReview)
			{
				if (ToReviewLightCurves[RecordNumber].IsAsteroid)
				{
					stringBuilder.Append("(" + ToReviewLightCurves[RecordNumber].AsteroidNumber + ") " + ToReviewLightCurves[RecordNumber].AsteroidName);
				}
				else
				{
					stringBuilder.Append("XZ" + ToDeleteLightCurves[RecordNumber].XZ);
					if (ToReviewLightCurves[RecordNumber].ZC > 0)
					{
						stringBuilder.Append("=ZC" + ToReviewLightCurves[RecordNumber].ZC);
					}
				}
				stringBuilder.Append(",  " + ToReviewLightCurves[RecordNumber].EventDate);
				stringBuilder.Append(",  " + ToReviewLightCurves[RecordNumber].Observer.Replace("/", " + "));
			}
			if (AddExtraHeader)
			{
				stringBuilder.Append("      " + HeaderExtraText);
			}
			Header = stringBuilder.ToString();
			if (!IsMultiplot)
			{
				Header = Header.Replace(",  ", ",    ");
			}
			Font font = ((!IsMultiplot) ? new Font("Arial", 12f, FontStyle.Bold, GraphicsUnit.Point) : new Font("Arial", 8f, FontStyle.Bold, GraphicsUnit.Point));
			Brush brush3 = Brushes.White;
			if (flag)
			{
				brush3 = Brushes.Wheat;
			}
			float width2 = graphics.MeasureString(Header, font).Width;
			float x = ((float)PlotWidth - width2) / 2f;
			if (!IsMultiplot)
			{
				graphics.FillRectangle(brush3, x, PlotTopHeight + 1, graphics.MeasureString(Header, font).Width, 16f);
				graphics.DrawString(Header, font, Brushes.DarkGreen, x, PlotTopHeight + 1);
			}
			else
			{
				graphics.FillRectangle(brush3, 4f, PlotTopHeight + 1, graphics.MeasureString(Header, font).Width, 16f);
				graphics.DrawString(Header, font, Brushes.Black, 4f, PlotTopHeight + 1);
			}
			string text = "";
			Brush brush4 = Brushes.White;
			Brush brush5 = Brushes.White;
			if (flag)
			{
				brush4 = (brush5 = Brushes.Wheat);
			}
			font = new Font("Arial", 8f, FontStyle.Regular, GraphicsUnit.Point);
			string text2 = string.Format("#{0,1}", RecordNumber + 1);
			float width3 = graphics.MeasureString(text2, font).Width;
			graphics.FillRectangle(brush4, (float)PlotWidth - width3 - 1f, PlotTopHeight + 1, width3, 16f);
			graphics.DrawString(text2, font, Brushes.Black, (float)PlotWidth - width3 - 1f, PlotTopHeight + 1);
			string text3 = "";
			if (Main)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", MainLightCurves[RecordNumber].Duration, MainLightCurves[RecordNumber].Duration / (double)(MainLightCurves[RecordNumber].NumPoints - 1));
			}
			else if (Mine)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", MyReportedLightCurves[RecordNumber].Duration, MyReportedLightCurves[RecordNumber].Duration / (double)(MyReportedLightCurves[RecordNumber].NumPoints - 1));
				text3 = MyReportedLightCurves[RecordNumber].FileName;
			}
			else if (Submitted)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", LightCurvesSubmitted[RecordNumber].Duration, LightCurvesSubmitted[RecordNumber].Duration / (double)(LightCurvesSubmitted[RecordNumber].NumPoints - 1));
				text3 = LightCurvesSubmitted[RecordNumber].FileName;
				if (LightCurvesSubmitted[RecordNumber].ReviewLightCurve)
				{
					brush4 = Brushes.Yellow;
				}
			}
			else if (Pending)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", MyPendingLightCurves[RecordNumber].Duration, MyPendingLightCurves[RecordNumber].Duration / (double)(MyPendingLightCurves[RecordNumber].NumPoints - 1));
				text3 = MyPendingLightCurves[RecordNumber].FileName;
			}
			else if (ToDelete)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", ToDeleteLightCurves[RecordNumber].Duration, ToDeleteLightCurves[RecordNumber].Duration / (double)(ToDeleteLightCurves[RecordNumber].NumPoints - 1));
				text3 = ToDeleteLightCurves[RecordNumber].FileName;
				if (ToDeleteLightCurves[RecordNumber].FileName.Contains(".init"))
				{
					brush4 = Brushes.LightGreen;
				}
				if (ToDeleteLightCurves[RecordNumber].FileName.Contains(".del"))
				{
					brush4 = Brushes.Pink;
				}
			}
			else if (ToReview)
			{
				text = string.Format("Durn {0,1:f3}s @ {1,1:f3}s", ToReviewLightCurves[RecordNumber].Duration, ToReviewLightCurves[RecordNumber].Duration / (double)(ToReviewLightCurves[RecordNumber].NumPoints - 1));
				text3 = ToReviewLightCurves[RecordNumber].FileName;
				brush4 = Brushes.Yellow;
			}
			font = new Font("Arial", 8f, FontStyle.Regular, GraphicsUnit.Point);
			num8 = graphics.MeasureString(text, font).Width;
			graphics.FillRectangle(brush5, 2f, PlotTopHeight + PlotHeight - 14, num8, 12f);
			graphics.DrawString(text, font, Brushes.Black, 2f, PlotTopHeight + PlotHeight - 15);
			font = new Font("Arial", 7f, FontStyle.Bold, GraphicsUnit.Point);
			float width4 = graphics.MeasureString(text3, font).Width;
			graphics.FillRectangle(brush4, (float)PlotWidth - width4, PlotTopHeight + PlotHeight - 12, width4, 10f);
			graphics.DrawString(text3, font, Brushes.Black, (float)PlotWidth - width4, PlotTopHeight + PlotHeight - 13);
			if (Main)
			{
				num7 = MainLightCurves[RecordNumber].NumPoints;
				num3 = (float)MainLightCurves[RecordNumber].Sec;
				num6 = (float)MainLightCurves[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = MainLightCurves[RecordNumber].StartTimeSecs;
				num9 = MainLightCurves[RecordNumber].Hr;
				num10 = MainLightCurves[RecordNumber].Min;
			}
			else if (Mine)
			{
				num7 = MyReportedLightCurves[RecordNumber].NumPoints;
				num3 = (float)MyReportedLightCurves[RecordNumber].Sec;
				num6 = (float)MyReportedLightCurves[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = MyReportedLightCurves[RecordNumber].StartTimeSecs;
				num9 = MyReportedLightCurves[RecordNumber].Hr;
				num10 = MyReportedLightCurves[RecordNumber].Min;
			}
			else if (Submitted)
			{
				num7 = LightCurvesSubmitted[RecordNumber].NumPoints;
				num3 = (float)LightCurvesSubmitted[RecordNumber].Sec;
				num6 = (float)LightCurvesSubmitted[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = LightCurvesSubmitted[RecordNumber].StartTimeSecs;
				num9 = LightCurvesSubmitted[RecordNumber].Hr;
				num10 = LightCurvesSubmitted[RecordNumber].Min;
			}
			else if (Pending)
			{
				num7 = MyPendingLightCurves[RecordNumber].NumPoints;
				num3 = (float)MyPendingLightCurves[RecordNumber].Sec;
				num6 = (float)MyPendingLightCurves[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = MyPendingLightCurves[RecordNumber].StartTimeSecs;
				num9 = MyPendingLightCurves[RecordNumber].Hr;
				num10 = MyPendingLightCurves[RecordNumber].Min;
			}
			else if (ToDelete)
			{
				num7 = ToDeleteLightCurves[RecordNumber].NumPoints;
				num3 = (float)ToDeleteLightCurves[RecordNumber].Sec;
				num6 = (float)ToDeleteLightCurves[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = ToDeleteLightCurves[RecordNumber].StartTimeSecs;
				num9 = ToDeleteLightCurves[RecordNumber].Hr;
				num10 = ToDeleteLightCurves[RecordNumber].Min;
			}
			else if (ToReview)
			{
				num7 = ToReviewLightCurves[RecordNumber].NumPoints;
				num3 = (float)ToReviewLightCurves[RecordNumber].Sec;
				num6 = (float)ToReviewLightCurves[RecordNumber].Duration;
				CurrentPlotStartTime_Secs = ToReviewLightCurves[RecordNumber].StartTimeSecs;
				num9 = ToReviewLightCurves[RecordNumber].Hr;
				num10 = ToReviewLightCurves[RecordNumber].Min;
			}
			Interval = num6 / (float)num7;
			CurrentPlotDuration = num6;
			double num14 = 1f / Interval;
			double num15 = (double)PlotScale * num14 / 80.0;
			float num16 = num3;
			float num17 = 1.5f;
			if (ThickLines && !IsMultiplot)
			{
				num17 *= 1.5f;
			}
			float y;
			float x2 = (y = 0f);
			int num18 = 1;
			int num19 = 1;
			bool flag2 = false;
			bool flag3 = false;
			int num20 = 0;
			for (int j = 0; j < num7; j++)
			{
				if (Main)
				{
					num18 = MainLightCurves[RecordNumber].LightValues[j];
					flag3 = MainLightCurves[RecordNumber].LightValuesValid[j];
				}
				else if (Mine)
				{
					num18 = MyReportedLightCurves[RecordNumber].LightValues[j];
					flag3 = MyReportedLightCurves[RecordNumber].LightValuesValid[j];
				}
				else if (Submitted)
				{
					num18 = LightCurvesSubmitted[RecordNumber].LightValues[j];
					flag3 = LightCurvesSubmitted[RecordNumber].LightValuesValid[j];
				}
				else if (Pending)
				{
					num18 = MyPendingLightCurves[RecordNumber].LightValues[j];
					flag3 = MyPendingLightCurves[RecordNumber].LightValuesValid[j];
				}
				else if (ToDelete)
				{
					num18 = ToDeleteLightCurves[RecordNumber].LightValues[j];
					flag3 = ToDeleteLightCurves[RecordNumber].LightValuesValid[j];
				}
				else if (ToReview)
				{
					num18 = ToReviewLightCurves[RecordNumber].LightValues[j];
					flag3 = ToReviewLightCurves[RecordNumber].LightValuesValid[j];
				}
				float num21 = (float)j * PlotScale;
				num13 = (float)(PlotTopHeight + PlotHeight) - (ZeroHeight + (float)num18 * num12);
				if (flag)
				{
					graphics.FillRectangle(brush2, num21 - PlotScale / 2f, (float)(PlotTopHeight + PlotHeight) - ZeroHeight - (float)num18 * num12, PlotScale, (float)num18 * num12 - 1f);
				}
				else
				{
					graphics.FillRectangle(brush, num21 - PlotScale / 2f, (float)(PlotTopHeight + PlotHeight) - ZeroHeight - (float)num18 * num12, PlotScale, (float)num18 * num12 - 1f);
				}
				if (flag3)
				{
					graphics.FillEllipse(darkBlue, num21 - num17, num13 - num17, 2f * num17, 2f * num17);
				}
				if (j > 0)
				{
					if (flag3 && flag2)
					{
						if ((num18 != 0 && num19 != 0) || !ShowSkippedFrames)
						{
							graphics.DrawLine(pen4, x2, y, num21, num13);
						}
						else if (ShowSkippedFrames)
						{
							graphics.DrawLine(pen6, x2, y, num21, num13);
						}
					}
					else if (ShowSkippedFrames)
					{
						graphics.DrawLine(pen5, x2, y, num21, num13);
					}
				}
				x2 = num21;
				y = num13;
				num19 = num18;
				flag2 = flag3;
			}
			num13 = (float)(PlotTopHeight + PlotHeight) - ZeroHeight;
			graphics.DrawLine(pen3, 0f, num13, PlotWidth, num13);
			font = ((!IsMultiplot) ? new Font("Arial", 8f, FontStyle.Bold, GraphicsUnit.Point) : new Font("Arial", 6f, FontStyle.Bold, GraphicsUnit.Point));
			num5 = num16;
			for (int k = 1; k < num7; k++)
			{
				num4 = num16 + Interval * (float)k;
				if (num15 > 3.0)
				{
					if (num4 * 2f % 1f < num5 * 2f % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text4 = string.Format("{0,1:f1}", (double)(int)(2f * num4) / 2.0);
						float height = graphics.MeasureString(text4, font).Height;
						if (2.5f * height < ZeroHeight)
						{
							graphics.DrawString(text4, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text4, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text4, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text4, font).Width / 2f, num13 - 3f - height);
						}
					}
				}
				else if (num15 > 1.0)
				{
					if (num4 % 1f < num5 % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text5 = string.Format("{0,1:f0}", (int)num4);
						float height2 = graphics.MeasureString(text5, font).Height;
						if (2.5f * height2 < ZeroHeight)
						{
							graphics.DrawString(text5, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text5, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text5, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text5, font).Width / 2f, num13 - 3f - height2);
						}
					}
				}
				else if (num15 > 0.5)
				{
					if (num4 / 2f % 1f < num5 / 2f % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text6 = string.Format("{0,1:f0}", (int)num4);
						float height3 = graphics.MeasureString(text6, font).Height;
						if (2.5f * height3 < ZeroHeight)
						{
							graphics.DrawString(text6, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text6, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text6, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text6, font).Width / 2f, num13 - 3f - height3);
						}
					}
				}
				else if (num15 > 0.2)
				{
					if (num4 / 5f % 1f < num5 / 5f % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text7 = string.Format("{0,1:f0}", (int)num4);
						float height4 = graphics.MeasureString(text7, font).Height;
						if (2.5f * height4 < ZeroHeight)
						{
							graphics.DrawString(text7, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text7, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text7, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text7, font).Width / 2f, num13 - 3f - height4);
						}
					}
				}
				else if (num15 > 0.1)
				{
					if (num4 / 10f % 1f < num5 / 10f % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text8 = string.Format("{0,1:f0}", (int)num4);
						float height5 = graphics.MeasureString(text8, font).Height;
						if (2.5f * height5 < ZeroHeight)
						{
							graphics.DrawString(text8, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text8, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text8, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text8, font).Width / 2f, num13 - 3f - height5);
						}
					}
				}
				else if (num15 > 0.05)
				{
					if (num4 / 20f % 1f < num5 / 20f % 1f)
					{
						graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
						string text9 = string.Format("{0,1:f0}", (int)num4);
						float height6 = graphics.MeasureString(text9, font).Height;
						if (2.5f * height6 < ZeroHeight)
						{
							graphics.DrawString(text9, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text9, font).Width / 2f, num13 + 3f);
						}
						else
						{
							graphics.DrawString(text9, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text9, font).Width / 2f, num13 - 3f - height6);
						}
					}
				}
				else if (num4 / 50f % 1f < num5 / 50f % 1f)
				{
					graphics.DrawLine(pen3, (float)k * PlotScale, num13 - 3f, (float)k * PlotScale, num13 + 3f);
					string text10 = string.Format("{0,1:f0}", (int)num4);
					float height7 = graphics.MeasureString(text10, font).Height;
					if (2.5f * height7 < ZeroHeight)
					{
						graphics.DrawString(text10, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text10, font).Width / 2f, num13 + 3f);
					}
					else
					{
						graphics.DrawString(text10, font, Brushes.Black, (float)k * PlotScale - graphics.MeasureString(text10, font).Width / 2f, num13 - 3f - height7);
					}
				}
				num5 = num4;
			}
			font = ((!IsMultiplot) ? new Font("Arial", 12f, FontStyle.Bold, GraphicsUnit.Point) : new Font("Arial", 7f, FontStyle.Regular, GraphicsUnit.Point));
			if (!IsMultiplot)
			{
				Utilities.DrawRotatedTextAt(graphics, -90f, "F L U X   (ADU)", 2, (int)((float)(PlotTopHeight + PlotHeight) - ZeroHeight - 15f), xy_isMiddleOfString: false, font, Brushes.DarkGreen, Brushes.White);
				string text11 = string.Format("UTC  {0,1}h {1,1}m  xx secs", num9, num10);
				graphics.DrawString(text11, font, Brushes.DarkGreen, ((float)PlotWidth - graphics.MeasureString(text11, font).Width) / 2f, (float)(PlotTopHeight + PlotHeight) - graphics.MeasureString(text11, font).Height);
			}
			double num22 = 0.0;
			for (int l = 0; l <= 1; l++)
			{
				if (!((l == 0) & !AddTimeMarker1) && !((l == 1) & !AddTimeMarker2))
				{
					num22 = ((l != 0) ? (HeaderExtraTime_UT2 - CurrentPlotStartTime_Secs) : (HeaderExtraTime_UT1 - CurrentPlotStartTime_Secs));
					if (num22 > 0.0 && num22 < (double)num6)
					{
						float num23 = (float)(num22 / (double)Interval);
						num13 = (float)(PlotTopHeight + PlotHeight) - ZeroHeight;
						float num24 = num13 - 20f;
						graphics.DrawLine(array[l], num23 * PlotScale, num24, num23 * PlotScale, num13);
						graphics.DrawLine(array[l], num23 * PlotScale, num24, num23 * PlotScale - 5f, num24 + 5f);
						graphics.DrawLine(array[l], num23 * PlotScale, num24, num23 * PlotScale + 5f, num24 + 5f);
					}
				}
			}
			if (num20 > 0)
			{
				string text12 = string.Format("{0,1} images missed", num20);
				font = new Font("Arial", 8f, FontStyle.Regular, GraphicsUnit.Point);
				float width5 = graphics.MeasureString(text12, font).Width;
				graphics.FillRectangle(brush4, num8 + 10f, PlotTopHeight + PlotHeight - 14, width5, 12f);
				graphics.DrawString(text12, font, Brushes.Red, num8 + 10f, PlotTopHeight + PlotHeight - 15);
			}
			graphics.DrawRectangle(pen, 0, PlotTopHeight, PlotWidth - 1, PlotHeight - 1);
			if (FirstIntegration_TruncationFrame >= 0 && FramesToCombine > 0)
			{
				for (int m = 0; m < num7; m++)
				{
					if ((m - FirstIntegration_TruncationFrame) % FramesToCombine == 0 && m >= FirstIntegration_TruncationFrame)
					{
						float num21 = ((float)m - 0.5f) * PlotScale;
						num13 = PlotTopHeight + PlotHeight;
						graphics.DrawLine(pen7, num21, PlotTopHeight, num21, num13);
					}
				}
			}
			graphics.Dispose();
			return bitmap;
		}
	}
}

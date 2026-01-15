using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Occult.Asteroids
{
	public class MutualAsteroidals : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private const double TwoPi = Math.PI * 2.0;

		private static double[] RANear;

		private static double[] DecNear;

		private static double[] DeltaNear;

		private static double[] RAFar;

		private static double[] DecFar;

		private static double[] DeltaFar;

		private static double MagNear = 30.0;

		private static double MagFar = 30.0;

		private static double CombinedMag = 30.0;

		private static double MagDrop = 0.0;

		private static double ErrMajorNear = 0.0;

		private static double ErrMajorFar = 0.0;

		private static double ErrMinorNear = 0.0;

		private static double ErrMinorFar = 0.0;

		private static double ErrPANear = 0.0;

		private static double ErrPAFar = 0.0;

		private static double ErrAlongNear = 0.0;

		private static double ErrAcrossNear = 0.0;

		private static double ErrAlongFar = 0.0;

		private static double ErrAcrossFar = 0.0;

		private static double ErrAlongNearFar = 0.0;

		private static double ErrAcrossNearFar = 0.0;

		private static string OrbitSourceNear = "";

		private static string OrbitSourceFar = "";

		private static string PathUncertainty = "";

		private static double DiameterNear;

		private static double DiameterFar;

		private static double RadiusNear = 1.0;

		private static double RadiusFar = 1.0;

		private static double DiameterUncertaintyNear;

		private static double DiameterUncertaintyFar;

		private static double ParallaxNear;

		private static double[] Xb;

		private static double[] Yb;

		private static double[] Zb;

		private static double[] Decb;

		private static double[] Mub;

		private static double[] L1b;

		private static double[] L2b;

		private static double[] SubstellarLongitude_OfDate;

		private static double[] SubSolarLongitude_OfDate;

		private static bool HorizonsError = false;

		private static bool Internet = Utilities.InternetIsAvailable();

		private static double[] X;

		private static double[] Y;

		private static double[] Z;

		private static double[] Dec;

		private static double[] Mu;

		private static double[] L1;

		private static double[] L2;

		private static double[] SubstellarLongitude;

		private static double[] SubSolarLongitude;

		private static int T0_OfSeries = 0;

		private static double Xt;

		private static double dXt = 0.0;

		private static double Yt = 0.0;

		private static double dYt;

		private static double Zt;

		private static double Dec_t;

		private static double Mu_t;

		private static double L1_t;

		private static double L2_t;

		private static double SubstellarLongitude_t;

		private static double SubSolarLongitude_t;

		private static double CentralDuration;

		private static double TotalDuration;

		private static string NearName = "";

		private static string FarName = "";

		private static string EventTitle = "";

		private static string EventDate = "";

		private static string CenterPathLabel = "";

		private static string ObjectCoords = "";

		private static string Circumstances = "";

		private static string SunMoon = "";

		private static string NearDetails = "";

		private static string FarDetails = "";

		private static string SourcePathRoot = "https://minorplanet.info/obsguides/appulses/Ast-Ast_";

		private static double n2;

		private static double D;

		private static double x1;

		private static double y1;

		private static double Gamma;

		private static double TConj = 0.0;

		private static double DecSun = 0.0;

		private const int ArrayHourAtZero = -2;

		private const string EOL = "\r\n";

		private int EventYear = 2000;

		private int EventMonth = 1;

		private int EventDay = 1;

		private int AsteroidNear = 1;

		private int AsteroidFar = 1;

		private int ArrayElementPriorToConjunction;

		private static AsteroidElements_All Asteroids;

		private static List<AsteroidPathCoords> PathCoordinates;

		private static AsteroidPathCoords CalculatedPath;

		private static DisplayData MutualPath;

		private IContainer components;

		private ListBox lstPossibleEvents;

		private Button cmdAppulses;

		private ComboBox cmbYear;

		private ListBox lstAppulseFiles;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label lblResult;

		private Label label1;

		private ComboBox cmbSearchDistance;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Panel panel1;

		private void MutualAsteroidals_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(336);
			if (((Control)this).get_Height() < 450)
			{
				((Control)this).set_Height(450);
			}
			((Control)panel1).set_Height(((Control)this).get_Height() - 39);
			((Control)lstPossibleEvents).set_Height(((Control)panel1).get_Height() - 281);
		}

		public MutualAsteroidals()
		{
			InitializeComponent();
			RANear = new double[30];
			DecNear = new double[30];
			DeltaNear = new double[30];
			RAFar = new double[30];
			DecFar = new double[30];
			DeltaFar = new double[30];
			Xb = new double[4];
			Yb = new double[4];
			Zb = new double[4];
			Decb = new double[4];
			Mub = new double[4];
			L1b = new double[4];
			L2b = new double[4];
			SubstellarLongitude_OfDate = new double[4];
			SubSolarLongitude_OfDate = new double[4];
			X = new double[4];
			Y = new double[4];
			Z = new double[4];
			Dec = new double[4];
			Mu = new double[4];
			L1 = new double[4];
			L2 = new double[4];
			SubstellarLongitude = new double[4];
			SubSolarLongitude = new double[4];
			((ListControl)lstPossibleEvents).set_SelectedIndex(0);
			for (int i = 2020; i < DateTime.Now.Year + 2; i++)
			{
				cmbYear.get_Items().Add((object)i);
			}
			((ListControl)cmbYear).set_SelectedIndex(cmbYear.get_Items().get_Count() - 2);
			((ListControl)cmbSearchDistance).set_SelectedIndex(4);
			ListAvailableAppulseFiles();
			Label obj = label3;
			Label obj2 = label7;
			bool internet;
			((Control)cmbSearchDistance).set_Enabled(internet = Internet);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = internet);
			((Control)obj).set_Enabled(enabled);
			((Control)label9).set_Visible(!Internet);
		}

		private void cmdAppulses_Click(object sender, EventArgs e)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			if (DownloadAppulses(Convert.ToInt32(cmbYear.get_Items().get_Item(((ListControl)cmbYear).get_SelectedIndex()).ToString())))
			{
				MessageBox.Show("Download sucessful", "Download", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			else
			{
				MessageBox.Show("Download failed or was incomplete", "Download", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			ListAvailableAppulseFiles();
		}

		internal void ListAvailableAppulseFiles()
		{
			lstAppulseFiles.set_Sorted(true);
			lstAppulseFiles.get_Items().Clear();
			FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Downloaded files").GetFiles("Appulses_*.dat");
			foreach (FileInfo fileInfo in files)
			{
				lstAppulseFiles.get_Items().Add((object)fileInfo.ToString().Replace(".dat", ""));
			}
			if (lstAppulseFiles.get_Items().get_Count() > 0)
			{
				((ListControl)lstAppulseFiles).set_SelectedIndex(lstAppulseFiles.get_Items().get_Count() - 1);
				ListPossibleEvents();
			}
		}

		private void lstAppulseFiles_Click(object sender, EventArgs e)
		{
			ListPossibleEvents();
		}

		private void ListPossibleEvents()
		{
			int result = 0;
			int num = Convert.ToInt32(cmbSearchDistance.get_Items().get_Item(((ListControl)cmbSearchDistance).get_SelectedIndex()).ToString()!.Replace("\"", ""));
			string text = "";
			string text2 = "";
			string text3 = lstAppulseFiles.get_Items().get_Item(((ListControl)lstAppulseFiles).get_SelectedIndex()).ToString();
			string path = Utilities.AppPath + "\\Downloaded files\\" + text3 + ".dat";
			string text4 = text3.Substring(text3.Length - 4);
			using (StreamReader streamReader = new StreamReader(path))
			{
				string text5 = streamReader.ReadLine();
				int num2 = text5.IndexOf("Date");
				int startIndex = num2 + 3;
				int num3 = text5.IndexOf("Ast 1");
				if (num3 < 0)
				{
					num3 = text5.IndexOf("Ast1") + 1;
				}
				int num4 = text5.IndexOf("Ast 2");
				if (num4 < 0)
				{
					num4 = text5.IndexOf("Ast2") + 1;
				}
				int startIndex2 = text5.IndexOf("Sep");
				lstPossibleEvents.get_Items().Clear();
				lstPossibleEvents.get_Items().Add((object)"Date       :   Near :    Far : Sepn");
				do
				{
					text = streamReader.ReadLine();
					int.TryParse(text.Substring(startIndex2, 3), out result);
					if (result < num)
					{
						text2 = text4 + " " + text.Substring(num2, 2) + " " + text.Substring(startIndex, 2) + " : " + text.Substring(num4, 6) + " : " + text.Substring(num3, 6) + " : " + text.Substring(startIndex2, 3) + "\"";
						lstPossibleEvents.get_Items().Add((object)text2);
					}
				}
				while (!streamReader.EndOfStream);
			}
			string text6 = "";
			string text7 = "";
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			bool flag = false;
			DirectoryInfo directoryInfo = new DirectoryInfo(Utilities.AppPath + "\\Predictions");
			for (int i = 1; i < lstPossibleEvents.get_Items().get_Count(); i++)
			{
				flag = false;
				string text8 = lstPossibleEvents.get_Items().get_Item(i).ToString();
				FileInfo[] files = directoryInfo.GetFiles("Mutual*.dat");
				for (int j = 0; j < files.Length; j++)
				{
					string text9 = files[j].ToString();
					num5 = text9.IndexOf("(");
					num6 = text9.IndexOf(")", num5);
					text6 = text9.Substring(num5 + 1, num6 - num5 - 1).Trim();
					num7 = text9.IndexOf("(", num6);
					num8 = text9.IndexOf(")", num7);
					text7 = text9.Substring(num7 + 1, num8 - num7 - 1).Trim();
					if (text8.Contains(text6) & text8.Contains(text7))
					{
						flag = true;
						break;
					}
				}
				TagPossibleEventLine(i, flag, FullCheck: false);
			}
		}

		private void TagPossibleEventLine(int Line, bool found, bool FullCheck)
		{
			string text = lstPossibleEvents.get_Items().get_Item(Line).ToString()!.Replace("✔", "").Replace("-", "").Trim();
			if (found)
			{
				lstPossibleEvents.get_Items().set_Item(Line, (object)(text + " ✔"));
			}
			else if (!FullCheck)
			{
				lstPossibleEvents.get_Items().set_Item(Line, (object)(text + " -"));
			}
			else
			{
				lstPossibleEvents.get_Items().set_Item(Line, (object)(text + " ✖"));
			}
		}

		private void lstAppulseFiles_DoubleClick(object sender, EventArgs e)
		{
			if (Internet)
			{
				for (int i = 1; i < lstPossibleEvents.get_Items().get_Count(); i++)
				{
					TagPossibleEventLine(i, PredictEvents(i), FullCheck: true);
				}
			}
		}

		private void lstPossibleEvents_Click(object sender, EventArgs e)
		{
			string text = "";
			string? text2 = lstPossibleEvents.get_Items().get_Item(((ListControl)lstPossibleEvents).get_SelectedIndex()).ToString();
			int num = text2!.IndexOf(":");
			int num2 = text2!.IndexOf(":", num + 1);
			string value = text2!.Substring(num + 1, num2 - num - 1).Trim();
			int num3 = text2!.IndexOf(":", num2 + 1);
			string value2 = text2!.Substring(num2 + 1, num3 - num2 - 1).Trim();
			for (int i = 1; i < lstPossibleEvents.get_Items().get_Count(); i++)
			{
				FileInfo[] files = new DirectoryInfo(Utilities.AppPath + "\\Predictions").GetFiles("Mutual_*.dat");
				for (int j = 0; j < files.Length; j++)
				{
					string text3 = files[j].ToString();
					if (text3.Contains(value) & text3.Contains(value2))
					{
						using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Predictions\\" + text3))
						{
							text = streamReader.ReadToEnd();
						}
						try
						{
							((Control)MutualPath).Show();
						}
						catch
						{
							MutualPath = new DisplayData();
							((Control)MutualPath).Show();
						}
						((Control)MutualPath).set_Width(1080);
						((Control)MutualPath.txtBox).set_Text(text);
						return;
					}
				}
			}
		}

		private void lstPossibleEvents_DoubleClick(object sender, EventArgs e)
		{
			if (Internet)
			{
				int selectedIndex = ((ListControl)lstPossibleEvents).get_SelectedIndex();
				if (selectedIndex > 0)
				{
					TagPossibleEventLine(selectedIndex, PredictEvents(selectedIndex), FullCheck: true);
				}
			}
		}

		private bool PredictEvents(int ListIndex)
		{
			//IL_0123: Unknown result type (might be due to invalid IL or missing references)
			bool result = false;
			try
			{
				((Form)MutualPath).Close();
			}
			catch
			{
			}
			string text = lstPossibleEvents.get_Items().get_Item(ListIndex).ToString();
			int.TryParse(text.Substring(0, 4), out EventYear);
			int.TryParse(text.Substring(5, 2), out EventMonth);
			int.TryParse(text.Substring(8, 2), out EventDay);
			int num = text.IndexOf(":");
			int num2 = text.IndexOf(":", num + 1);
			int.TryParse(text.Substring(num + 1, num2 - num - 1), out AsteroidNear);
			int num3 = text.IndexOf(":", num2 + 1);
			int.TryParse(text.Substring(num2 + 1, num3 - num2 - 1), out AsteroidFar);
			if (AsteroidNear == 134340)
			{
				return false;
			}
			if (!http.GetHorizons_Asteroid_30hr_ApparentEphemeris_Less2_Plus27(AsteroidNear, EventYear, EventMonth, EventDay, UseTTnotUT: false, out RANear, out DecNear, out DeltaNear, out MagNear, out ErrMajorNear, out ErrMinorNear, out ErrPANear))
			{
				MessageBox.Show("Horizons ephemeris not available", "Horizons error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				HorizonsError = true;
				return false;
			}
			http.GetHorizons_Asteroid_30hr_ApparentEphemeris_Less2_Plus27(AsteroidFar, EventYear, EventMonth, EventDay, UseTTnotUT: false, out RAFar, out DecFar, out DeltaFar, out MagFar, out ErrMajorFar, out ErrMinorFar, out ErrPAFar);
			if (DeltaNear[14] > DeltaFar[14])
			{
				Utilities.Swap(ref AsteroidNear, ref AsteroidFar);
				http.GetHorizons_Asteroid_30hr_ApparentEphemeris_Less2_Plus27(AsteroidNear, EventYear, EventMonth, EventDay, UseTTnotUT: false, out RANear, out DecNear, out DeltaNear, out MagNear, out ErrMajorNear, out ErrMinorNear, out ErrPANear);
				http.GetHorizons_Asteroid_30hr_ApparentEphemeris_Less2_Plus27(AsteroidFar, EventYear, EventMonth, EventDay, UseTTnotUT: false, out RAFar, out DecFar, out DeltaFar, out MagFar, out ErrMajorFar, out ErrMinorFar, out ErrPAFar);
			}
			Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(AsteroidNear, out DiameterNear, out DiameterUncertaintyNear, out var Source);
			Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(AsteroidFar, out DiameterFar, out DiameterUncertaintyFar, out Source);
			RadiusNear = DiameterNear / 2.0;
			RadiusFar = DiameterFar / 2.0;
			AsteroidElements AE = new AsteroidElements();
			http.GetHorizonsElements_TDBtime(AsteroidNear.ToString(), EventYear, EventMonth, EventDay, 0.0, ref AE);
			OrbitSourceNear = AE.OrbitSource + ":" + AE.OrbitDate;
			NearName = "(" + AE.IDNumber + ") " + AE.IDName.Trim();
			NearDetails = NearName + string.Format(",   Mag = {0,1:f2}", MagNear) + string.Format(",   Dia = {0,1:f1} km", DiameterNear) + string.Format(" = {0,1:f1} mas,   Dist = {1,1:f3} AU,   Uncert. {2,1:f0} x {3,1:f0} mas in PA {4,1:f0}°,   Orbit: ", 8794.143836182533 / DeltaNear[14] * DiameterNear / 12756.274, DeltaNear[14], ErrMajorNear * 1000.0, ErrMinorNear * 1000.0, ErrPANear) + OrbitSourceNear;
			AE = new AsteroidElements();
			http.GetHorizonsElements_TDBtime(AsteroidFar.ToString(), EventYear, EventMonth, EventDay, 0.0, ref AE);
			OrbitSourceFar = AE.OrbitSource + ":" + AE.OrbitDate;
			FarName = "(" + AE.IDNumber + ") " + AE.IDName.Trim();
			FarDetails = FarName + string.Format(",   Mag = {0,1:f2}", MagFar) + string.Format(",   Dia = {0,1:f1} km", DiameterFar) + string.Format(" = {0,1:f1} mas,   Dist = {1,1:f3} AU,   Uncert. {2,1:f0} x {3,1:f0} mas in PA {4,1:f0}°,   Orbit: ", 8794.143836182533 / DeltaFar[14] * DiameterFar / 12756.274, DeltaFar[14], ErrMajorFar * 1000.0, ErrMinorFar * 1000.0, ErrPAFar) + OrbitSourceFar;
			ArrayElementPriorToConjunction = FindArrayElementPriorToConjunction();
			bool flag = MutualBessellianElements(EventYear, EventMonth, EventDay, ArrayElementPriorToConjunction, AsteroidNear, AsteroidFar, IncludeMaps: false);
			string text2 = string.Format("⊕ = {0,1:f3}   Path width = {1,1:f1} km  ", Gamma, L1[0] * 6378.137);
			if (L2[0] > 0.0)
			{
				Circumstances = "Transit";
			}
			else
			{
				Circumstances = "Occultation";
			}
			Circumstances += string.Format(":  Total mag = {0,1:f2}   Mag drop = {1,1:f2}   Central duration = {2,1:f2} sec   Total duration = {3,1:f2} sec   Path widths = {4,1:f1} km & {5,1:f1} km", CombinedMag, MagDrop, CentralDuration, TotalDuration, Math.Abs(L2[0]) * 6378.137, L1[0] * 6378.137);
			((Control)lblResult).set_Text(text + "\r\n" + text2);
			if (flag)
			{
				AsteroidPath();
				string text3 = "";
				if (PathCoordinates.Count > 0)
				{
					text3 += EventDate;
					text3 = text3 + "     " + EventTitle + "\r\n";
					text3 = text3 + "\r\n" + Circumstances + "\r\n";
					text3 += "\r\nAsteroid details\r\n";
					text3 = text3 + ObjectCoords + "\r\n";
					text3 = text3 + NearDetails + "\r\n";
					text3 = text3 + FarDetails + "\r\n";
					text3 = text3 + PathUncertainty + "\r\n";
					text3 += "\r\n";
					text3 = text3 + "                   Centre                            Sun " + CenterPathLabel + "\r\n";
					text3 += "  E. Longitude    Latitude      U.T.     Alt    Az   Alt  Longitude   Latitude  Longitude   Latitude  Longitude   Latitude  Longitude   Latitude\r\n";
					text3 += "      o  '  \"     o  '  \"    h  m  s       o     o     o    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"    o  '  \"\r\n";
					for (int i = 0; i < PathCoordinates.Count; i++)
					{
						if (i % 5 == 0)
						{
							text3 += "\r\n";
						}
						text3 = text3 + PathCoordinates[i].ToString().Substring(0, 144) + "\r\n";
					}
					text3 = text3 + "".PadRight(144, '_') + "\r\n";
					string text4 = Utilities.AppPath + "\\Predictions\\Mutual_" + EventYear + EventMonth.ToString().PadLeft(2, '0') + EventDay.ToString().PadLeft(2, '0') + "_" + EventTitle.Replace("  ", "_").Replace(" ", "");
					using (StreamWriter streamWriter = new StreamWriter(text4 + ".dat"))
					{
						streamWriter.Write(text3);
					}
					try
					{
						((Control)MutualPath).Show();
					}
					catch
					{
						MutualPath = new DisplayData();
						((Control)MutualPath).Show();
					}
					((Control)MutualPath).set_Width(1080);
					((Control)MutualPath.txtBox).set_Text(text3);
					CreateGoogleEarthKMLFile(text4 + ".kmz", EventTitle, Auto: true, View: true);
					Label obj3 = lblResult;
					((Control)obj3).set_Text(((Control)obj3).get_Text() + "  An event");
					result = true;
				}
				else
				{
					Label obj4 = lblResult;
					((Control)obj4).set_Text(((Control)obj4).get_Text() + "  Close, but no event");
				}
			}
			else
			{
				Label obj5 = lblResult;
				((Control)obj5).set_Text(((Control)obj5).get_Text() + "  No event");
			}
			return result;
		}

		internal int FindArrayElementPriorToConjunction()
		{
			double[] array = new double[30];
			for (int i = 0; i < 30; i++)
			{
				array[i] = 0.0;
			}
			for (int j = 0; j < 30; j++)
			{
				Utilities.Distance(RANear[j], DecNear[j], RAFar[j], DecFar[j], out array[j], out var _);
			}
			for (int k = 1; k < 29; k++)
			{
				if (Math.Sign(array[k] - array[k - 1]) != Math.Sign(array[k + 1] - array[k]))
				{
					if (array[k - 1] > array[k + 1])
					{
						return k;
					}
					return k - 1;
				}
			}
			return -1;
		}

		public static bool MutualBessellianElements(int Year, int Month, int Day, int ArrayElementPriorToConjunction, int NearAsteroid, int FarAsteroid, bool IncludeMaps)
		{
			double num = 0.0;
			double num2 = 0.0;
			double RA = 0.0;
			double Distance = 0.0;
			double Distance2 = 0.0;
			double Distance3 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			string text = "+";
			double num5 = Utilities.JD_from_Date(Year, Month, Day);
			double num6 = Utilities.SiderealTime_deg(num5, Apparent: true);
			for (int i = 0; i <= 3; i++)
			{
				int num7 = ArrayElementPriorToConjunction + i - 1;
				double num8 = DeltaFar[num7] * Math.Cos(DecFar[num7]) * Math.Cos(RAFar[num7]) - DeltaNear[num7] * Math.Cos(DecNear[num7]) * Math.Cos(RANear[num7]);
				double num9 = DeltaFar[num7] * Math.Cos(DecFar[num7]) * Math.Sin(RAFar[num7]) - DeltaNear[num7] * Math.Cos(DecNear[num7]) * Math.Sin(RANear[num7]);
				double num10 = DeltaFar[num7] * Math.Sin(DecFar[num7]) - DeltaNear[num7] * Math.Sin(DecNear[num7]);
				num2 = Math.Atan2(num9, num8);
				Decb[i] = Math.Atan(num10 / Math.Sqrt(num8 * num8 + num9 * num9));
				double num11 = Math.Sqrt(num8 * num8 + num9 * num9 + num10 * num10);
				ParallaxNear = 8.794143836182533 / DeltaNear[num7] / 3600.0 / (180.0 / Math.PI);
				Xb[i] = Math.Cos(DecNear[num7]) * Math.Sin(RANear[num7] - num2) / Math.Sin(ParallaxNear);
				Yb[i] = (Math.Sin(DecNear[num7]) * Math.Cos(Decb[i]) - Math.Cos(DecNear[num7]) * Math.Sin(Decb[i]) * Math.Cos(RANear[num7] - num2)) / Math.Sin(ParallaxNear);
				Zb[i] = (Math.Sin(DecNear[num7]) * Math.Sin(Decb[i]) + Math.Cos(DecNear[num7]) * Math.Cos(Decb[i]) * Math.Cos(RANear[num7] - num2)) / Math.Sin(ParallaxNear);
				L1b[i] = Zb[i] * ((RadiusFar + RadiusNear) / num11 / 149597870.7) + RadiusNear / 6378.137;
				L2b[i] = Zb[i] * ((RadiusFar - RadiusNear) / num11 / 149597870.7) - RadiusNear / 6378.137;
				num = (num6 + 15.041067 * (double)(num7 + -2)) % 360.0;
				Mub[i] = num / (180.0 / Math.PI) - num2;
				while (Mub[i] < 0.0)
				{
					Mub[i] += Math.PI * 2.0;
				}
				while (Mub[i] > Math.PI * 2.0)
				{
					Mub[i] -= Math.PI * 2.0;
				}
				if (i > 0 && Mub[i] < Mub[0])
				{
					Mub[i] += Math.PI * 2.0;
				}
				SubstellarLongitude_OfDate[i] = 180.0 / Math.PI * RAFar[num7] - num;
				while (SubstellarLongitude_OfDate[i] > 180.0)
				{
					SubstellarLongitude_OfDate[i] -= 360.0;
				}
				while (SubstellarLongitude_OfDate[i] < -180.0)
				{
					SubstellarLongitude_OfDate[i] += 360.0;
				}
				if (i > 0 && SubstellarLongitude_OfDate[i] > SubstellarLongitude_OfDate[0])
				{
					SubstellarLongitude_OfDate[i] -= Math.PI * 2.0;
				}
				Utilities.QuickPlanet(num5 + (double)(num7 + -2) / 24.0, 3, EquinoxOfDate: true, out RA, out DecSun, out var GeocentricDistance);
				SubSolarLongitude_OfDate[i] = 180.0 / Math.PI * RA - num;
				while (SubSolarLongitude_OfDate[i] > 180.0)
				{
					SubSolarLongitude_OfDate[i] -= 360.0;
				}
				while (SubSolarLongitude_OfDate[i] < -180.0)
				{
					SubSolarLongitude_OfDate[i] += 360.0;
				}
				if (i > 0 && SubSolarLongitude_OfDate[i] > SubSolarLongitude_OfDate[0])
				{
					SubSolarLongitude_OfDate[i] -= Math.PI * 2.0;
				}
				if (i == 0)
				{
					Utilities.Distance(RA, DecSun, num2, Decb[0], out Distance, out GeocentricDistance);
					Utilities.QuickMoon(num5 + (double)ArrayElementPriorToConjunction / 24.0, out var RA2, out var Dec, out GeocentricDistance, out var _, out var MoonLatitude);
					Utilities.Distance(RA2, Dec, num2, Decb[0], out Distance3, out MoonLatitude);
					Utilities.Distance(RA2, Dec, RA, DecSun, out Distance2, out MoonLatitude);
					num3 = 50.0 * (1.0 - Math.Cos(Distance2));
					num4 = RA2 - RA;
					if (num4 < 0.0)
					{
						num4 += Math.PI * 2.0;
					}
					text = "+";
					if (num4 > Math.PI)
					{
						text = "-";
					}
				}
			}
			T0_OfSeries = ArrayElementPriorToConjunction + -2;
			Utilities.FitCubicTo4_EqualSpaced_Points(Xb[0], Xb[1], Xb[2], Xb[3], out X[0], out X[1], out X[2], out X[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(Yb[0], Yb[1], Yb[2], Yb[3], out Y[0], out Y[1], out Y[2], out Y[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(Zb[0], Zb[1], Zb[2], Zb[3], out Z[0], out Z[1], out Z[2], out Z[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(Mub[0], Mub[1], Mub[2], Mub[3], out Mu[0], out Mu[1], out Mu[2], out Mu[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(Decb[0], Decb[1], Decb[2], Decb[3], out MutualAsteroidals.Dec[0], out MutualAsteroidals.Dec[1], out MutualAsteroidals.Dec[2], out MutualAsteroidals.Dec[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(L1b[0], L1b[1], L1b[2], L1b[3], out L1[0], out L1[1], out L1[2], out L1[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(L2b[0], L2b[1], L2b[2], L2b[3], out L2[0], out L2[1], out L2[2], out L2[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(SubstellarLongitude_OfDate[0], SubstellarLongitude_OfDate[1], SubstellarLongitude_OfDate[2], SubstellarLongitude_OfDate[3], out SubstellarLongitude[0], out SubstellarLongitude[1], out SubstellarLongitude[2], out SubstellarLongitude[3]);
			Utilities.FitCubicTo4_EqualSpaced_Points(SubSolarLongitude_OfDate[0], SubSolarLongitude_OfDate[1], SubSolarLongitude_OfDate[2], SubSolarLongitude_OfDate[3], out SubSolarLongitude[0], out SubSolarLongitude[1], out SubSolarLongitude[2], out SubSolarLongitude[3]);
			n2 = X[1] * X[1] + Y[1] * Y[1];
			D = X[0] * X[1] + Y[0] * Y[1];
			double num12 = (0.0 - D) / n2;
			x1 = X[0] + X[1] * num12;
			y1 = Y[0] + Y[1] * num12;
			Gamma = Math.Sqrt(x1 * x1 + y1 * y1);
			TConj = (double)(ArrayElementPriorToConjunction + -2) + num12;
			_ = (X[0] * Y[1] - Y[0] * X[1]) / Math.Sqrt(n2);
			EventDate = Year + " " + Utilities.ShortMonths[Month] + " " + Day;
			if (L2b[0] < 0.0)
			{
				EventTitle = NearName + "  occults  " + FarName;
				CenterPathLabel = "<---- Path limits of full occultation ----> <-- Path limits of partial occultation --->";
			}
			else
			{
				EventTitle = NearName + "  transits  " + FarName;
				CenterPathLabel = "<------ Path limits of full transit ------> <---- Path limits of partial transit ----->";
			}
			CombinedMag = -2.5 * Math.Log10(Math.Pow(10.0, MagNear / -2.5) + Math.Pow(10.0, MagFar / -2.5));
			if (MagNear > MagFar)
			{
				MagDrop = MagFar - CombinedMag;
			}
			else
			{
				MagDrop = MagNear - CombinedMag;
			}
			CentralDuration = Math.Abs(L2b[0]) / Math.Sqrt(n2) * 3600.0 * 2.0;
			TotalDuration = L1b[0] / Math.Sqrt(n2) * 3600.0 * 2.0;
			Utilities.ApparentPositionToJ2000(num5, RANear[ArrayElementPriorToConjunction], DecNear[ArrayElementPriorToConjunction], out var RA3, out var Dec2);
			ObjectCoords = "RA = " + Utilities.DEGtoDMS(RA3 * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: false, IncludeDMS: false, IncludeHMS: true) + "   Dec = " + Utilities.DEGtoDMS(Dec2 * (180.0 / Math.PI), 3, 0, MinutesOnly: false, IncludeLeadingZeros: false, IncludePlusSymbol: true, IncludeDMS: true, IncludeHMS: false) + string.Format("  (J2000)          Elongations:  Sun {0,1:f0}°,  Moon {1,1:f0}° (illum. {2,1:f0}%{3})", Distance * (180.0 / Math.PI), Distance3 * (180.0 / Math.PI), num3, text);
			double num13 = Math.Atan2(X[1], Y[1]) * (180.0 / Math.PI);
			if (num13 > 180.0)
			{
				num13 -= 180.0;
			}
			if (num13 < 0.0)
			{
				num13 += 180.0;
			}
			double num14 = ErrMajorFar / 8.794143836182533 * DeltaFar[ArrayElementPriorToConjunction];
			double num15 = ErrMinorFar / 8.794143836182533 * DeltaFar[ArrayElementPriorToConjunction];
			double num16 = ErrMajorNear / 8.794143836182533 * DeltaNear[ArrayElementPriorToConjunction];
			double num17 = ErrMinorNear / 8.794143836182533 * DeltaNear[ArrayElementPriorToConjunction];
			ErrAlongFar = (ErrAcrossFar = (ErrAlongNear = (ErrAcrossNear = -100.0)));
			for (double num18 = 0.0; num18 < 361.0; num18 += 8.0)
			{
				double x = num14 * Math.Sin(num18 / (180.0 / Math.PI));
				double y = num15 * Math.Cos(num18 / (180.0 / Math.PI));
				Utilities.RotateXY(x, y, ErrPAFar - num13, out var x2, out var y2);
				if (x2 > ErrAlongFar)
				{
					ErrAlongFar = x2;
				}
				if (y2 > ErrAcrossFar)
				{
					ErrAcrossFar = y2;
				}
				double x3 = num16 * Math.Sin(num18 / (180.0 / Math.PI));
				y = num17 * Math.Cos(num18 / (180.0 / Math.PI));
				Utilities.RotateXY(x3, y, ErrPANear - num13, out x2, out y2);
				if (x2 > ErrAlongNear)
				{
					ErrAlongNear = x2;
				}
				if (y2 > ErrAcrossNear)
				{
					ErrAcrossNear = y2;
				}
			}
			ErrAlongNearFar = Utilities.QuadratureAddition(ErrAlongNear, ErrAlongFar);
			ErrAcrossNearFar = Utilities.QuadratureAddition(ErrAcrossNear, ErrAcrossFar);
			double num19 = 0.3;
			if (Gamma < 0.95)
			{
				num19 = Math.Sqrt(1.0 - Gamma * Gamma);
			}
			PathUncertainty = string.Format("Path uncertainties:    Event time:  ±{0,1:f2} secs,    Across-path distance on Earth's surface: ±{1,1:f0} km", ErrAlongNearFar / Math.Sqrt(n2) * 3600.0, ErrAcrossNearFar * 6378.137 / num19);
			if (Gamma > 1.01 + L1[0])
			{
				return false;
			}
			return true;
		}

		internal static void ParametersAtTimeT(double t)
		{
			double num = t - (double)T0_OfSeries;
			double num2 = num * num;
			double num3 = num * num * num;
			Xt = X[0] + X[1] * num + X[2] * num2 + X[3] * num3;
			dXt = X[1] + 2.0 * X[2] * num + 3.0 * X[3] * num2;
			Yt = Y[0] + Y[1] * num + Y[2] * num2 + Y[3] * num3;
			dYt = Y[1] + 2.0 * Y[2] * num + 3.0 * Y[3] * num2;
			Zt = Z[0] + Z[1] * num + Z[2] * num2 + Z[3] * num3;
			Dec_t = Dec[0] + Dec[1] * num + Dec[2] * num2 + Dec[3] * num3;
			Mu_t = Mu[0] + Mu[1] * num + Mu[2] * num2 + Mu[3] * num3;
			L1_t = L1[0] + L1[1] * num + L1[2] * num2 + L1[3] * num3;
			L2_t = L2[0] + L2[1] * num + L2[2] * num2 + L2[3] * num3;
			SubstellarLongitude_t = SubstellarLongitude[0] + SubstellarLongitude[1] * num + SubstellarLongitude[2] * num2 + SubstellarLongitude[3] * num3;
			SubSolarLongitude_t = SubSolarLongitude[0] + SubSolarLongitude[1] * num + SubSolarLongitude[2] * num2 + SubSolarLongitude[3] * num3;
		}

		internal static void AsteroidPath()
		{
			bool ValidPath = false;
			ParametersAtTimeT(TConj);
			PathCoordinates = new List<AsteroidPathCoords>();
			CalculatedPath = new AsteroidPathCoords();
			double num = Math.Abs((Xt * Y[1] - Yt * X[1]) / Math.Sqrt(n2));
			double num2 = 1.0 + L1_t;
			if (!(num < num2))
			{
				return;
			}
			double num3 = 1.3 * (Math.Sqrt(num2 * num2 - num * num) / Math.Sqrt(n2));
			double num4 = 1.0;
			if (Math.Abs(num) < 1.4)
			{
				num4 = Math.Sqrt(1.0 - num * num / 2.0);
			}
			double num5 = 50.0 * num4;
			for (double num6 = 0.0 - num3; num6 <= num3; num6 += num3 / num5)
			{
				PathByTime(num6, ref CalculatedPath, out ValidPath);
				if (ValidPath)
				{
					PathCoordinates.Add(CalculatedPath);
				}
			}
		}

		internal static void PathByTime(double TfromConj, ref AsteroidPathCoords CalculatedPath, out bool ValidPath)
		{
			ValidPath = false;
			CalculatedPath = new AsteroidPathCoords();
			CalculatedPath.IterateOnT = true;
			CalculatedPath.IterateOnLatitude = false;
			CalculatedPath.PathIsTopographic = false;
			for (int i = -2; i <= 2; i++)
			{
				CentrePathByTime(TfromConj, i, out var Valid, out var LatitudeDeg, out var LongitudeDeg);
				if (!Valid)
				{
					continue;
				}
				ValidPath = true;
				switch (i)
				{
				case -2:
					CalculatedPath.LongitudeRightSigma = LongitudeDeg;
					CalculatedPath.LatitudeRightSigma = LatitudeDeg;
					break;
				case -1:
					CalculatedPath.LongitudeRight = LongitudeDeg;
					CalculatedPath.LatitudeRight = LatitudeDeg;
					break;
				case 0:
				{
					CalculatedPath.LongitudeCentre = LongitudeDeg;
					CalculatedPath.LatitudeCentre = LatitudeDeg;
					CalculatedPath.TCentre = TConj + TfromConj;
					CalculatedPath.CentreLineValid = true;
					double num = Math.Atan(Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Tan(LatitudeDeg / (180.0 / Math.PI)));
					double num2 = Math.Cos(num);
					double num3 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Sin(num);
					double num4 = (0.0 - SubstellarLongitude_t) / (180.0 / Math.PI) + 0.2625161707907961 * TfromConj + LongitudeDeg / (180.0 / Math.PI);
					double y = (0.0 - Math.Cos(Dec_t)) * Math.Sin(num4);
					double x = num2 * Math.Sin(Dec_t) - num3 * Math.Cos(num4) * Math.Cos(Dec_t);
					double num5 = 180.0 / Math.PI * Math.Atan2(y, x);
					if (num5 < 0.0)
					{
						num5 += 360.0;
					}
					CalculatedPath.StarAz = num5;
					CalculatedPath.StarAlt = 180.0 / Math.PI * Math.Asin(num3 * Math.Sin(Dec_t) + num2 * Math.Cos(num4) * Math.Cos(Dec_t));
					CalculatedPath.SunAlt = 180.0 / Math.PI * Math.Asin(num3 * Math.Sin(DecSun) + num2 * Math.Cos(num4 + (SubstellarLongitude_t - SubSolarLongitude_t) / (180.0 / Math.PI)) * Math.Cos(DecSun));
					break;
				}
				case 1:
					CalculatedPath.LongitudeLeft = LongitudeDeg;
					CalculatedPath.LatitudeLeft = LatitudeDeg;
					break;
				case 2:
					CalculatedPath.LongitudeLeftSigma = LongitudeDeg;
					CalculatedPath.LatitudeLeftSigma = LatitudeDeg;
					break;
				}
			}
		}

		internal static void CentrePathByTime(double TfromConjunction, int LimitID, out bool Valid, out double LatitudeDeg, out double LongitudeDeg)
		{
			Valid = false;
			LatitudeDeg = (LongitudeDeg = 0.0);
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			LatitudeDeg = (LongitudeDeg = 0.0);
			ParametersAtTimeT(TConj + TfromConjunction);
			if (LimitID != 0)
			{
				if (Math.Abs(LimitID) == 2)
				{
					num3 = L1_t;
				}
				else if (Math.Abs(LimitID) == 1)
				{
					num3 = Math.Abs(L2_t);
				}
				double num4 = Math.Atan2(dYt, dXt);
				num = num3 * Math.Cos(num4 - Math.PI / 2.0) * (double)Math.Sign(LimitID);
				num2 = num3 * Math.Sin(num4 - Math.PI / 2.0) * (double)Math.Sign(LimitID);
			}
			double num5 = Math.Sin(Dec_t);
			double num6 = Utilities.sqrt_1LessEarthEllipticitySqrd * Math.Cos(Dec_t);
			double num7 = Math.Sqrt(num5 * num5 + num6 * num6);
			double num8 = num5 / num7;
			double num9 = num6 / num7;
			double num10 = (0.0 - SubstellarLongitude_t) / (180.0 / Math.PI) + 0.2625161707907961 * TfromConjunction;
			double num11 = Xt + num;
			double num12 = Yt + num2;
			double num13 = 1.0 - num11 * num11 - num12 * num12;
			Valid = false;
			if (num13 > 0.0)
			{
				Valid = true;
				double num14 = Math.Sqrt(num13);
				double num15 = num11;
				double num16 = (0.0 - num12) * num8 + num14 * num9;
				double num17 = num12 * num9 + num14 * num8;
				LatitudeDeg = 180.0 / Math.PI * Math.Atan(num17 / Math.Sqrt(num15 * num15 + num16 * num16) / Utilities.sqrt_1LessEarthEllipticitySqrd);
				for (LongitudeDeg = 180.0 / Math.PI * (Math.Atan2(num15, num16) - num10); LongitudeDeg < -180.0; LongitudeDeg += 360.0)
				{
				}
				while (LongitudeDeg > 180.0)
				{
					LongitudeDeg -= 360.0;
				}
			}
		}

		internal static void CreateGoogleEarthKMLFile(string OutFile, string Label, bool Auto, bool View)
		{
			double longitude = 0.0;
			double latitude = 0.0;
			int[] array = new int[5] { 2, 3, 3, 1, 1 };
			bool flag = false;
			int num = 1;
			if (PathCoordinates.Count < 1 || !GoogleEarth.Create_New_GoogleEarthKMZ_File(OutFile, Label, Auto, out var CreatedFile))
			{
				return;
			}
			for (num = -1; num <= 3; num++)
			{
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(array[1 + num]);
				for (int i = 0; i < PathCoordinates.Count; i++)
				{
					switch (num)
					{
					case -1:
						longitude = PathCoordinates[i].LongitudeCentre;
						latitude = PathCoordinates[i].LatitudeCentre;
						flag = PathCoordinates[i].CentreLineValid;
						break;
					case 0:
						longitude = PathCoordinates[i].LongitudeLeft;
						latitude = PathCoordinates[i].LatitudeLeft;
						flag = PathCoordinates[i].LatitudeLeft < 100.0;
						break;
					case 1:
						longitude = PathCoordinates[i].LongitudeRight;
						latitude = PathCoordinates[i].LatitudeRight;
						flag = PathCoordinates[i].LatitudeRight < 100.0;
						break;
					case 2:
						longitude = PathCoordinates[i].LongitudeLeftSigma;
						latitude = PathCoordinates[i].LatitudeLeftSigma;
						flag = PathCoordinates[i].LatitudeLeftSigma < 100.0;
						break;
					case 3:
						longitude = PathCoordinates[i].LongitudeRightSigma;
						latitude = PathCoordinates[i].LatitudeRightSigma;
						flag = PathCoordinates[i].LatitudeRightSigma < 100.0;
						break;
					}
					if (flag)
					{
						GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(longitude, latitude, 0.0);
					}
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			if (View)
			{
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		internal static bool DownloadAppulses(int Year)
		{
			if ((Year < 2020) | (Year > DateTime.Now.Year + 1))
			{
				return false;
			}
			int num = 0;
			int num2 = 0;
			string text = "";
			for (int i = 1; i <= 12; i++)
			{
				string text2 = http.Download_WebPage(SourcePathRoot + Utilities.Months[i] + Year + ".html");
				if (text2.Length == 0)
				{
					return false;
				}
				if (i == 1)
				{
					num = text2.IndexOf("Date");
					num2 = text2.IndexOf("\r", num);
					text = text + text2.Substring(num, num2 - num) + "\r\n";
				}
				num = text2.IndexOf("--\r") + 4;
				num2 = text2.IndexOf("<", num);
				text += text2.Substring(num, num2 - num);
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\DownLoaded Files\\Appulses_" + Year + ".dat"))
			{
				streamWriter.Write(text);
			}
			return true;
		}

		private void MutualAsteroidals_HelpButtonClicked(object sender, CancelEventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Mutual asteroids");
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			lstPossibleEvents = new ListBox();
			cmdAppulses = new Button();
			cmbYear = new ComboBox();
			lstAppulseFiles = new ListBox();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			lblResult = new Label();
			label1 = new Label();
			cmbSearchDistance = new ComboBox();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			panel1 = new Panel();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstPossibleEvents).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPossibleEvents).set_FormattingEnabled(true);
			lstPossibleEvents.get_Items().AddRange(new object[1] { "Date       :   Near :    Far :  Sepn" });
			((Control)lstPossibleEvents).set_Location(new Point(30, 202));
			((Control)lstPossibleEvents).set_Name("lstPossibleEvents");
			lstPossibleEvents.set_ScrollAlwaysVisible(true);
			((Control)lstPossibleEvents).set_Size(new Size(257, 199));
			((Control)lstPossibleEvents).set_TabIndex(1);
			lstPossibleEvents.add_Click((EventHandler)lstPossibleEvents_Click);
			((Control)lstPossibleEvents).add_DoubleClick((EventHandler)lstPossibleEvents_DoubleClick);
			((Control)cmdAppulses).set_Location(new Point(106, 25));
			((Control)cmdAppulses).set_Name("cmdAppulses");
			((Control)cmdAppulses).set_Size(new Size(170, 30));
			((Control)cmdAppulses).set_TabIndex(4);
			((Control)cmdAppulses).set_Text("Download appulses for the year");
			((ButtonBase)cmdAppulses).set_UseVisualStyleBackColor(true);
			((Control)cmdAppulses).add_Click((EventHandler)cmdAppulses_Click);
			((ListControl)cmbYear).set_FormattingEnabled(true);
			((Control)cmbYear).set_Location(new Point(30, 30));
			((Control)cmbYear).set_Name("cmbYear");
			((Control)cmbYear).set_Size(new Size(59, 21));
			((Control)cmbYear).set_TabIndex(5);
			((ListControl)lstAppulseFiles).set_FormattingEnabled(true);
			((Control)lstAppulseFiles).set_Location(new Point(30, 81));
			((Control)lstAppulseFiles).set_Name("lstAppulseFiles");
			((Control)lstAppulseFiles).set_Size(new Size(104, 95));
			lstAppulseFiles.set_Sorted(true);
			((Control)lstAppulseFiles).set_TabIndex(6);
			lstAppulseFiles.add_Click((EventHandler)lstAppulseFiles_Click);
			((Control)lstAppulseFiles).add_DoubleClick((EventHandler)lstAppulseFiles_DoubleClick);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(30, 65));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(78, 13));
			((Control)label2).set_TabIndex(7);
			((Control)label2).set_Text("Available years");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(141, 113));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(129, 39));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("Double-click to compute\r\nevents having an appulse\r\nseparation less than ");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(30, 187));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(114, 13));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text("List of possible events.");
			((Control)lblResult).set_Anchor((AnchorStyles)6);
			((Control)lblResult).set_AutoSize(true);
			((Control)lblResult).set_Location(new Point(30, 404));
			((Control)lblResult).set_Name("lblResult");
			((Control)lblResult).set_Size(new Size(37, 13));
			((Control)lblResult).set_TabIndex(10);
			((Control)lblResult).set_Text("Result");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(37, 15));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(11);
			((Control)label1).set_Text("Year");
			((ListControl)cmbSearchDistance).set_FormattingEnabled(true);
			cmbSearchDistance.get_Items().AddRange(new object[8] { "40\"", "35\"", "30\"", "25\"", "20\"", "15\"", "10\"", "5\"" });
			((Control)cmbSearchDistance).set_Location(new Point(241, 140));
			((Control)cmbSearchDistance).set_Name("cmbSearchDistance");
			((Control)cmbSearchDistance).set_Size(new Size(41, 21));
			((Control)cmbSearchDistance).set_TabIndex(12);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(149, 153));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(0, 13));
			((Control)label5).set_TabIndex(13);
			((Control)label6).set_Anchor((AnchorStyles)6);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(30, 441));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(267, 26));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("Prediction files stored in the subdirectory /Predictions/, \r\nwith file names beginning with 'Mutual_\"");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(181, 177));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(106, 24));
			((Control)label7).set_TabIndex(15);
			((Control)label7).set_Text("Double-click to compute \r\nan individual event");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(141, 81));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(92, 13));
			((Control)label8).set_TabIndex(16);
			((Control)label8).set_Text("Click to list events");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.Red);
			((Control)label9).set_Location(new Point(141, 100));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(70, 13));
			((Control)label9).set_TabIndex(17);
			((Control)label9).set_Text("No internet");
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)cmbSearchDistance);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)lblResult);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)lstAppulseFiles);
			((Control)panel1).get_Controls().Add((Control)(object)cmbYear);
			((Control)panel1).get_Controls().Add((Control)(object)cmdAppulses);
			((Control)panel1).get_Controls().Add((Control)(object)lstPossibleEvents);
			((Control)panel1).set_Location(new Point(0, 0));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(316, 480));
			((Control)panel1).set_TabIndex(18);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(316, 480));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_HelpButton(true);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("MutualAsteroidals");
			((Control)this).set_Text("Mutual asteroid events");
			((Form)this).add_HelpButtonClicked((CancelEventHandler)MutualAsteroidals_HelpButtonClicked);
			((Control)this).add_Resize((EventHandler)MutualAsteroidals_Resize);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}

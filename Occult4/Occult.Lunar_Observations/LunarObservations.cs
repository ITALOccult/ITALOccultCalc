using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Ephemerides;
using Occult.Mapping;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class LunarObservations
	{
		public static string AppPath;

		internal static string CurrentSourceFile = "";

		internal static OccultationReport OccMain = new OccultationReport();

		internal static OccultationReport OccMerge = new OccultationReport();

		private static ChooseStar CS;

		internal static bool EditsNotSaved = true;

		internal static bool SavedToArchive = false;

		private const double Radian = 180.0 / Math.PI;

		private const double twoPi = Math.PI * 2.0;

		private const double RadianSec = 648000.0 / Math.PI;

		private const double PE_Adjustment = 2.3148148148148148E-06;

		internal const string VizierFor_dT = "Vizier Archive before 1970";

		private static ArchiveEditor Archive_Editor;

		public static AnalyseForStarPosition AnalyseStars;

		internal static CameraCorrections CameraCorrections;

		private static DoubleList DoubleList;

		internal static DoublesSolve DoublesSolve;

		internal static DoubleStarReport DoubleStarReport;

		private static ErrorReport Errors;

		private static GrazeSummary Graze_Summary;

		private static Grazes HistoricalGrazes;

		internal static HistoricalOccultations HistoricalOccults;

		internal static ListResiduals List_Residuals;

		internal static ListDuplicates ArchiveDuplicates;

		public static GlobalMaintenance ProcessRegionalArchiveFiles;

		internal static MissingGrazes MissingGrazes;

		public static Magnitudes MagnitudeCalculator;

		private static ObservationsEditor Observations_Editor;

		private static EventTimeFromStarAltitude UTfromStarAlt;

		private static PBar Progress;

		internal static List<PossibleStars> StarList = new List<PossibleStars>();

		internal static PossibleStars PossibleStarID;

		internal static List<ReductionLine> Residuals = new List<ReductionLine>();

		internal static ReductionLine Reduction_Line;

		internal static int IDLine;

		internal static int EventCode;

		internal static string SearchText = "";

		internal static string EventCodeForIdentification = "";

		public static void Show_ArchiveEditor()
		{
			try
			{
				((Control)Archive_Editor).Show();
			}
			catch
			{
				Archive_Editor = new ArchiveEditor();
				((Control)Archive_Editor).Show();
			}
			((Control)Archive_Editor).Focus();
		}

		public static void Show_CameraCorrections()
		{
			try
			{
				((Control)CameraCorrections).Show();
			}
			catch
			{
				CameraCorrections = new CameraCorrections();
				((Control)CameraCorrections).Show();
			}
			((Control)CameraCorrections).Focus();
		}

		public static void Show_DoublesSolve()
		{
			try
			{
				((Control)DoublesSolve).Show();
			}
			catch
			{
				DoublesSolve = new DoublesSolve();
				((Control)DoublesSolve).Show();
			}
			((Control)DoublesSolve).Focus();
		}

		public static void Show_DoubleStarReport()
		{
			try
			{
				((Control)DoubleStarReport).Show();
			}
			catch
			{
				DoubleStarReport = new DoubleStarReport();
				((Control)DoubleStarReport).Show();
			}
			((Control)DoubleStarReport).Focus();
		}

		public static void Show_ObservationsEditor()
		{
			try
			{
				((Control)Observations_Editor).Show();
			}
			catch
			{
				Observations_Editor = new ObservationsEditor();
				((Control)Observations_Editor).Show();
			}
			((Control)Observations_Editor).Focus();
		}

		public static void Show_ListResiduals()
		{
			try
			{
				((Control)List_Residuals).Show();
			}
			catch
			{
				List_Residuals = new ListResiduals();
				((Control)List_Residuals).Show();
			}
			((Control)List_Residuals).Focus();
		}

		public static void Show_HistoricalGrazes()
		{
			try
			{
				((Control)HistoricalGrazes).Show();
			}
			catch
			{
				HistoricalGrazes = new Grazes();
				((Control)HistoricalGrazes).Show();
			}
			((Control)HistoricalGrazes).Focus();
		}

		internal static void Show_GrazeSummary()
		{
			try
			{
				((Control)Graze_Summary).Show();
			}
			catch
			{
				Graze_Summary = new GrazeSummary();
				((Control)Graze_Summary).Show();
			}
			((Control)Graze_Summary).Focus();
		}

		public static void Show_MissingGrazes()
		{
			try
			{
				((Control)MissingGrazes).Show();
			}
			catch
			{
				MissingGrazes = new MissingGrazes();
				((Control)MissingGrazes).Show();
			}
		}

		public static void Show_HistoricalOccults()
		{
			try
			{
				((Control)HistoricalOccults).Show();
			}
			catch
			{
				HistoricalOccults = new HistoricalOccultations();
				((Control)HistoricalOccults).Show();
			}
			((Control)HistoricalOccults).Focus();
		}

		public static void ShowGlobalLunarProcessing()
		{
			try
			{
				((Control)ProcessRegionalArchiveFiles).Show();
			}
			catch
			{
				ProcessRegionalArchiveFiles = new GlobalMaintenance();
				((Control)ProcessRegionalArchiveFiles).Show();
			}
		}

		public static void Show_MagnitudeCalculator()
		{
			try
			{
				((Control)MagnitudeCalculator).Show();
			}
			catch
			{
				MagnitudeCalculator = new Magnitudes();
				((Control)MagnitudeCalculator).Show();
			}
		}

		internal static void ShowDuplicates()
		{
			try
			{
				((Control)ArchiveDuplicates).Show();
			}
			catch
			{
				ArchiveDuplicates = new ListDuplicates();
				((Control)ArchiveDuplicates).Show();
			}
			ArchiveDuplicates.SetForArchiving(ForArchiving: false);
			((Control)ArchiveDuplicates).Focus();
		}

		public static void ShowStarAnalysis()
		{
			try
			{
				((Control)AnalyseStars).Show();
			}
			catch
			{
				AnalyseStars = new AnalyseForStarPosition();
				((Control)AnalyseStars).Show();
			}
			((Control)AnalyseStars).Focus();
		}

		public static void Show_UTfromStarAlt()
		{
			try
			{
				((Control)UTfromStarAlt).Show();
			}
			catch
			{
				UTfromStarAlt = new EventTimeFromStarAltitude();
				((Control)UTfromStarAlt).Show();
			}
			((Control)UTfromStarAlt).Focus();
		}

		public static void CloseAllLunarObservationForms()
		{
			try
			{
				((Form)ArchiveDuplicates).Close();
				((Component)(object)ArchiveDuplicates).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Archive_Editor).Close();
				((Component)(object)Archive_Editor).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)AnalyseStars).Close();
				((Component)(object)AnalyseStars).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)DoubleList).Close();
				((Component)(object)DoubleList).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)DoublesSolve).Close();
				((Component)(object)DoublesSolve).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)DoubleStarReport).Close();
				((Component)(object)DoubleStarReport).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Errors).Close();
				((Component)(object)Errors).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Graze_Summary).Close();
				((Component)(object)Graze_Summary).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)HistoricalGrazes).Close();
				((Component)(object)HistoricalGrazes).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)HistoricalOccults).Close();
				((Component)(object)HistoricalOccults).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)List_Residuals).Close();
				((Component)(object)List_Residuals).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MagnitudeCalculator).Close();
				((Component)(object)MagnitudeCalculator).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)MissingGrazes).Close();
				((Component)(object)MissingGrazes).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)Observations_Editor).Close();
				((Component)(object)Observations_Editor).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)ProcessRegionalArchiveFiles).Close();
				((Component)(object)ProcessRegionalArchiveFiles).Dispose();
			}
			catch
			{
			}
			try
			{
				((Form)ReductionProfile.ObservedProfile).Close();
				((Component)(object)ReductionProfile.ObservedProfile).Dispose();
			}
			catch
			{
			}
		}

		internal static void IdentifyXZStar(double JD_atEvent, double Longitude, double Latitude, double Height, int DatumNumber, string EventPhase, double DistanceCriterion, bool fromAutoCorrect, bool fromManualDetect)
		{
			//IL_04e5: Unknown result type (might be due to invalid IL or missing references)
			string text = AppPath + "\\Resource Files\\XZ80.dat";
			bool flag = false;
			StarList.Clear();
			EventCodeForIdentification = EventPhase;
			double num = (JD_atEvent - 2451545.0) / 365.25;
			Utilities.TopocentricMoon(JD_atEvent - 0.0001, Longitude, Latitude, Height, Altitude_is_MSL: true, DatumNumber, out var RA, out var Dec, out var MoonRadius_Radians, out var MoonScale, out var l, out var b, out var C, out var MoonAlt_Deg);
			Utilities.TopocentricMoon(JD_atEvent, Longitude, Latitude, Height, Altitude_is_MSL: true, DatumNumber, out var RA2, out var Dec2, out MoonRadius_Radians, out MoonScale, out l, out b, out C, out MoonAlt_Deg);
			double num2 = 90.0 - Math.Atan((Dec2 - Dec) / (RA2 - RA) / Math.Cos(Dec2)) * (180.0 / Math.PI);
			double num3 = 0.0;
			double num4 = 0.0;
			double RA3;
			double Dec3;
			do
			{
				RA3 = RA2 - num3;
				Dec3 = Dec2 - num4;
				Utilities.ApparentStarPosition(ref RA3, ref Dec3, 0.0, 0.0, 2000, JD_atEvent, use2006values_Not1976: false);
				num3 += RA3 - RA2;
				if (num3 > 6.0)
				{
					num3 -= Math.PI * 2.0;
				}
				if (num3 < -6.0)
				{
					num3 += Math.PI * 2.0;
				}
				num4 += Dec3 - Dec2;
			}
			while (Math.Abs(RA3 - RA2) > 3E-06);
			RA3 = RA2 - num3;
			Dec3 = Dec2 - num4;
			int num5 = (int)Math.Floor(RA3 * (180.0 / Math.PI) - 0.3);
			int num6 = (int)Math.Floor(RA3 * (180.0 / Math.PI) + 0.3) + 2;
			if (num5 < 1)
			{
				num5 += 360;
			}
			if (num6 < 1)
			{
				num6 += 360;
			}
			if (num5 >= 360)
			{
				num5 -= 360;
			}
			if (num6 >= 360)
			{
				num6 -= 360;
			}
			LunarOccultations.InitialiseStarCatIndexArray(text);
			FileStream fileStream = new FileStream(text, FileMode.Open, FileAccess.Read);
			BinaryReader readXZ = new BinaryReader(fileStream);
			int num7 = LunarOccultations.XZIndex[num5];
			int num8 = LunarOccultations.XZIndex[num6];
			int num9 = LunarOccultations.XZIndex[360];
			int num10 = num7;
			int num11 = num8;
			if (num8 < num7)
			{
				num10 = 1;
				num11 = num9;
			}
			fileStream.Seek(num10 * 35, SeekOrigin.Begin);
			for (int i = num10; i <= num11; i++)
			{
				if (num8 < num7 && i == num8)
				{
					i = num7;
					fileStream.Seek(i * 35, SeekOrigin.Begin);
				}
				XZ80Q.ReadStarEntry(fileStream, readXZ, i);
				double rA_OfTarget = XZ80Q.RA_rad + num * XZ80Q.PMRA_rad;
				double dec_Target = XZ80Q.Dec_rad + num * XZ80Q.PMDec_rad;
				Utilities.Distance(RA3, Dec3, rA_OfTarget, dec_Target, out var Distance, out var PA_atOrigin);
				double num12 = (Distance - MoonRadius_Radians) * (180.0 / Math.PI) * 3600.0;
				if (!(Math.Abs(num12) < DistanceCriterion))
				{
					continue;
				}
				double num13 = PA_atOrigin * (180.0 / Math.PI) - num2;
				if (num13 > 180.0)
				{
					num13 = 360.0 - num13;
				}
				if (num13 < -180.0)
				{
					num13 = 360.0 + num13;
				}
				num13 = Math.Abs(num13);
				flag = false;
				if (EventPhase == "D")
				{
					if (num13 <= 95.0)
					{
						flag = true;
					}
				}
				else if (EventPhase == "R")
				{
					if (num13 >= 85.0)
					{
						flag = true;
					}
				}
				else if (num13 >= 85.0 && num13 <= 95.0)
				{
					flag = true;
				}
				if (flag)
				{
					if (Math.Abs(num12) < 5.0 && Utilities.LOLAFileExists)
					{
						num12 -= Utilities.LOLA_LimbHeight_ActualDistance(PA_atOrigin * (180.0 / Math.PI) - C, l, b, MoonScale);
					}
					PossibleStarID = new PossibleStars();
					PossibleStarID.Mag = XZ80Q.Mv;
					PossibleStarID.PA = PA_atOrigin * (180.0 / Math.PI);
					PossibleStarID.Residual = num12;
					PossibleStarID.XZ = XZ80Q.XZ;
					if (XZ80Q.ZC > 0)
					{
						PossibleStarID.Number = "R" + XZ80Q.ZC;
					}
					else if (XZ80Q.SAO > 0)
					{
						PossibleStarID.Number = "S" + XZ80Q.SAO;
					}
					else
					{
						PossibleStarID.Number = "X" + XZ80Q.XZ;
					}
					StarList.Add(PossibleStarID);
				}
			}
			fileStream.Close();
			PossibleStars.SortFlag = 0;
			StarList.Sort();
			IDLine = -1;
			if (fromAutoCorrect)
			{
				return;
			}
			try
			{
				CS = new ChooseStar();
			}
			catch
			{
			}
			((Form)CS).set_DialogResult((DialogResult)1);
			((Form)CS).ShowDialog();
			if (fromManualDetect)
			{
				if (IDLine >= 0)
				{
					((Control)Archive_Editor.txtStar).set_Text(StarList[IDLine].Number.Substring(1));
					Archive_Editor.optZC.set_Checked(StarList[IDLine].Number.Substring(0, 1) == "R");
					Archive_Editor.optSAO.set_Checked(StarList[IDLine].Number.Substring(0, 1) == "S");
					Archive_Editor.optXZ.set_Checked(StarList[IDLine].Number.Substring(0, 1) == "X");
				}
			}
			else if (IDLine >= 0)
			{
				((Control)Observations_Editor.txtXZ).set_Text(StarList[IDLine].XZ.ToString());
				if (((ListControl)Observations_Editor.cmbDoubles).get_SelectedIndex() == 7)
				{
					((ListControl)Observations_Editor.cmbDoubles).set_SelectedIndex(0);
				}
			}
		}

		internal static void IdentifyXZstarsInReport()
		{
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0091: Invalid comparison between Unknown and I4
			//IL_0098: Unknown result type (might be due to invalid IL or missing references)
			//IL_009e: Invalid comparison between Unknown and I4
			Observations_Editor.optEvents.set_Checked(true);
			Observations_Editor.SetEditFrame();
			for (int i = 0; i < OccMain.Events.Count; i++)
			{
				if ((OccMain.Events[i].StarNumber < 1) | !"RSXPA".Contains(OccMain.Events[i].StarCat))
				{
					((ListControl)Observations_Editor.lstEvents).set_SelectedIndex(i);
					Observations_Editor.DisplayEventsInEditor(i);
					Observations_Editor.IdentifyStar();
					if ((int)((Form)CS).get_DialogResult() == 3)
					{
						break;
					}
					if ((int)((Form)CS).get_DialogResult() == 1 && ((OccMain.Events[i].StarNumber < 1) | !"RSXPA".Contains(OccMain.Events[i].StarCat)))
					{
						Observations_Editor.EncodeEvent(i);
						EditsNotSaved = true;
						SavedToArchive = false;
					}
				}
			}
			Observations_Editor.DisplayReport(AllForms: true, HighlightLast: false);
		}

		internal static bool Reduce_CurrentEntry(double JD_atEvent, int TelescopeLine, string StarCat, int StarNumber, bool PE_AdjustIfBrightStar, string WDScode, out double Residual, out double PA, out double MagStar, out double MoonRadius)
		{
			double l;
			double b;
			double AA;
			string CA;
			double Illum;
			double StarAlt;
			return Reduce_CurrentEntry_Librations(JD_atEvent, TelescopeLine, StarCat, StarNumber, PE_AdjustIfBrightStar, WDScode, ApplyLimbCorrection: true, out Residual, out PA, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: false, out CA, out Illum, out StarAlt);
		}

		internal static bool Reduce_CurrentEntry_Librations(double JD_atEvent, int TelescopeLine, string StarCat, int StarNumber, bool PE_AdjustIfBrightStar, string WDScode, bool ApplyLimbCorrection, out double Residual, out double PA, out double MagStar, out double MoonRadius, out double l, out double b, out double AA, bool Include_LimbIllumAlt, out string CA, out double Illum, out double StarAlt)
		{
			double RA_Planet = 0.0;
			double Dec_Planet = 0.0;
			double PlanetRadius = 0.0;
			double P_Limb_deg = 0.0;
			double D_Limb_deg = 0.0;
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			double MoonAlt_Deg = (Residual = (PA = (MagStar = (MoonRadius = (l = (b = (AA = (Illum = (StarAlt = 0.0)))))))));
			CA = " ";
			if (TelescopeLine < 0 || StarNumber < 1)
			{
				return false;
			}
			double longitude = OccMain.Telescopes[TelescopeLine].Longitude;
			double latitude = OccMain.Telescopes[TelescopeLine].Latitude;
			double altitude = OccMain.Telescopes[TelescopeLine].Altitude;
			bool altitude_is_MSL = OccMain.Telescopes[TelescopeLine].VerticalDatum == "M";
			int num4 = OccMain.Telescopes[TelescopeLine].DatumNumber;
			if (num4 > 0 && num4 < 6)
			{
				num4 = Utilities.RGOdatum_to_ILOCdatum(num4, longitude * (180.0 / Math.PI), latitude * (180.0 / Math.PI));
			}
			Utilities.EarthOrientationParameters(JD_atEvent, out var x, out var y, out var _);
			longitude += (x * Math.Cos(longitude) - y * Math.Sin(longitude)) / 3600.0 / (180.0 / Math.PI);
			latitude += (x * Math.Sin(longitude) + y * Math.Cos(longitude)) * Math.Tan(latitude) / 3600.0 / (180.0 / Math.PI);
			bool flag = false;
			if (!(StarCat == "P"))
			{
				flag = StarCat == "A" || XZ80Q.Get_XZ_Star(StarNumber);
			}
			else
			{
				num = StarNumber / 1000;
				num2 = StarNumber % 100;
				flag = true;
			}
			if (flag)
			{
				double Delta;
				string SatName;
				float SatDia;
				if (num3 > 0)
				{
					num3 = StarNumber;
					http.GetHorizons_Asteroid_or_Satellite_ApparentPosition(GetAsteroid: true, num3, 0, 0, JD_atEvent, UseTTnotUT: false, out var RA, out var Dec, out Delta, out SatName, out SatDia, out var _, out var _);
					Utilities.TopocentricPosition(JD_atEvent, RA, Dec, 4.26352124542639E-05 / Delta, longitude, latitude, altitude, out RA_Planet, out Dec_Planet);
				}
				else if (num == 0)
				{
					RA_Planet = XZ80Q.RA_rad;
					Dec_Planet = XZ80Q.Dec_rad;
					MagStar = XZ80Q.Mv;
					if (XZ80Q.Mv <= 4.0 && PE_AdjustIfBrightStar)
					{
						JD_atEvent += 2.3148148148148148E-06;
					}
					if ((XZ80Q.DoubleFlag != " ") & (WDScode == " "))
					{
						WDScode = DoubleStars.ComponentIfMeanPosition(XZ80Q.XZ);
					}
					if (WDScode != " ")
					{
						Get_DoubleStar_Sep_PA(XZ80Q.XZ, JD_atEvent, WDScode, out var ComponentSep, out var ComponentPA);
						RA_Planet += ComponentSep * Math.Sin(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI) / Math.Cos(Dec_Planet);
						Dec_Planet += ComponentSep * Math.Cos(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI);
					}
					Utilities.ApparentStarPosition(ref RA_Planet, ref Dec_Planet, XZ80Q.PMRA_rad, XZ80Q.PMDec_rad, 2000, JD_atEvent, use2006values_Not1976: false);
				}
				else if (num2 == 0)
				{
					Utilities.TopocentricPlanet(JD_atEvent, num, longitude, latitude, altitude, out RA_Planet, out Dec_Planet, out PlanetRadius);
					PlanetRadius *= 648000.0 / Math.PI;
				}
				else
				{
					http.GetHorizons_Asteroid_or_Satellite_ApparentPosition(GetAsteroid: false, 0, num, num2, JD_atEvent, UseTTnotUT: false, out var RA2, out var Dec2, out Delta, out SatName, out SatDia, out var _, out var _);
					Utilities.TopocentricPosition(JD_atEvent, RA2, Dec2, 4.26352124542639E-05 / Delta, longitude, latitude, altitude, out RA_Planet, out Dec_Planet);
				}
				Utilities.TopocentricMoon(JD_atEvent, longitude, latitude, altitude, altitude_is_MSL, num4, out var RA3, out var Dec3, out MoonRadius, out var MoonScale, out l, out b, out var C, out MoonAlt_Deg);
				Utilities.Distance(RA3, Dec3, RA_Planet, Dec_Planet, out var Distance, out PA);
				AA = PA * (180.0 / Math.PI) - C;
				double SlopeBefore_Deg;
				double SlopeAfter_Deg;
				double num5 = ((!Utilities.LOLAFileExists) ? 0.0 : LOLAHiRes.LimbHeight_Slope(AA, l, b, 1.0, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg));
				Residual = (Distance - MoonRadius) * (180.0 / Math.PI) * 3600.0;
				if (ApplyLimbCorrection)
				{
					Residual -= MoonScale * num5;
					if (num > 0 && num2 == 0)
					{
						Residual += PlanetRadius;
					}
				}
				if (Include_LimbIllumAlt)
				{
					StarAlt = Utilities.Altitude(JD_atEvent, RA_Planet, Dec_Planet, longitude, latitude) * (180.0 / Math.PI);
					Utilities.QuickPlanet(JD_atEvent, 3, EquinoxOfDate: true, out var RA4, out var Dec4, out var _);
					double num6 = RA4 - RA3;
					if (num6 > Math.PI)
					{
						num6 -= Math.PI * 2.0;
					}
					if (num6 < -Math.PI)
					{
						num6 += Math.PI * 2.0;
					}
					double y2 = Math.Cos(Dec4) * Math.Sin(num6);
					double x2 = Math.Sin(Dec4) * Math.Cos(Dec3) - Math.Cos(Dec4) * Math.Sin(Dec3) * Math.Cos(num6);
					double num7;
					for (num7 = Math.Atan2(y2, x2) * (180.0 / Math.PI) - 90.0; num7 > 90.0; num7 -= 180.0)
					{
					}
					for (; num7 < -90.0; num7 += 180.0)
					{
					}
					double num8 = (double)Math.Sign(num6) * (num7 - PA * (180.0 / Math.PI));
					if (num8 > 180.0)
					{
						num8 -= 360.0;
					}
					if (num8 < -180.0)
					{
						num8 += 360.0;
					}
					CA = "N";
					if (Math.Abs(num8) > 90.0)
					{
						num8 = (double)Math.Sign(num8) * (180.0 - Math.Abs(num8));
						CA = "S";
					}
					CA = $"{num8:0;-0}" + CA;
					double num9 = Math.Sin(Dec4) * Math.Sin(Dec3) + Math.Cos(Dec4) * Math.Cos(Dec3) * Math.Cos(num6);
					Illum = 100.0 * (0.5 - num9 / 2.0);
				}
			}
			return flag;
		}

		internal static bool ReduceAnObservation(double JD_atEvent, double Longitude, double Latitude, double Height, bool Height_is_MSL, int DatumNumber, string StarCat, int StarNumber, bool UseOuterLimbOfSatelliteOrAsteroid, bool PE_ReductionIfBrightStar, string WDScode, bool ApplyLimbCorrn, bool VizierReduction, out string Reduction, out double Residual, out double MoonAlt, out double SunAlt, out double Illumination, out double MagStar, out double PA, out double MoonRadius)
		{
			double num = 0.0;
			double P_Limb_deg = 0.0;
			double D_Limb_deg = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double RA_end = 0.0;
			double Dec_end = 0.0;
			double num4 = 0.0;
			double PlanetRadius = 0.0;
			double RA_Planet = 0.0;
			double Dec_Planet = 0.0;
			double Distance = 0.0;
			double num5 = 0.0;
			float SatDia = 0f;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			string text = "";
			Residual = -9999.0;
			MoonAlt = (SunAlt = (Illumination = 0.0));
			double MoonRadius_Radians = (PA = (MagStar = 0.0));
			Reduction = "";
			if (DatumNumber > 0 && DatumNumber < 6)
			{
				DatumNumber = Utilities.RGOdatum_to_ILOCdatum(DatumNumber, Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI));
			}
			Utilities.EarthOrientationParameters(JD_atEvent, out var x, out var y, out var _);
			Longitude += (x * Math.Cos(Longitude) - y * Math.Sin(Longitude)) / 3600.0 / (180.0 / Math.PI);
			Latitude += (x * Math.Sin(Longitude) + y * Math.Cos(Longitude)) * Math.Tan(Latitude) / 3600.0 / (180.0 / Math.PI);
			bool flag = false;
			switch (StarCat)
			{
			case "P":
				num6 = StarNumber / 1000;
				num7 = StarNumber % 100;
				flag = true;
				break;
			case "A":
				num8 = StarNumber;
				flag = true;
				break;
			case "R":
				flag = XZ80Q.Get_ZC_Star(StarNumber);
				break;
			case "S":
				flag = XZ80Q.Get_SAO_Star(StarNumber);
				break;
			default:
				flag = XZ80Q.Get_XZ_Star(StarNumber);
				break;
			}
			double Delta;
			string SatName;
			if (num6 == 0 && num8 == 0)
			{
				XZ80Q.GetGaiaData(XZ80Q.DoubleFlag);
				num2 = XZ80Q.RA_rad;
				num3 = XZ80Q.Dec_rad;
				double pMRA_rad = XZ80Q.PMRA_rad;
				double pMDec_rad = XZ80Q.PMDec_rad;
				MagStar = XZ80Q.Mv;
				num4 = XZ80Q.Parallax_Rad;
				text = XZ80Q.PositionSource;
				if (text.Substring(0, 4) == "Gaia")
				{
					text = text.Substring(4);
				}
				if (XZ80Q.Mv <= 4.0 && PE_ReductionIfBrightStar)
				{
					JD_atEvent += 2.3148148148148148E-06;
				}
				if ((XZ80Q.DoubleFlag != " ") & (WDScode == " "))
				{
					WDScode = DoubleStars.ComponentIfMeanPosition(XZ80Q.XZ);
				}
				if (WDScode != " ")
				{
					Get_DoubleStar_Sep_PA(XZ80Q.XZ, JD_atEvent, WDScode, out var ComponentSep, out var ComponentPA);
					num2 += ComponentSep * Math.Sin(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI) / Math.Cos(Dec_end);
					num3 += ComponentSep * Math.Cos(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI);
				}
				Utilities.ApparentStarPosition(num2, num3, pMRA_rad, pMDec_rad, num4, 2000, IncludeRelativisticBending: true, JD_atEvent, use2006values_Not1976: true, out RA_end, out Dec_end);
				RA_Planet = RA_end;
				Dec_Planet = Dec_end;
			}
			else if (StarCat == "P")
			{
				if (num7 == 0)
				{
					Utilities.TopocentricPlanet(JD_atEvent, num6, Longitude, Latitude, Height, out RA_end, out Dec_end, out PlanetRadius);
					Utilities.TopocentricPlanet(JD_atEvent + 1.1574074074074073E-05, num6, Longitude, Latitude, Height, out RA_Planet, out Dec_Planet, out MoonRadius_Radians);
					PlanetRadius *= 648000.0 / Math.PI;
					text = Utilities.EphemerisBasis().Substring(0, 6);
				}
				else
				{
					http.GetHorizons_Asteroid_or_Satellite_ApparentPosition(GetAsteroid: false, 0, num6, num7, JD_atEvent, UseTTnotUT: false, out var RA, out var Dec, out Delta, out SatName, out SatDia, out var _, out var _);
					Utilities.TopocentricPosition(JD_atEvent, RA, Dec, 4.26352124542639E-05 / Delta, Longitude, Latitude, Height, out RA_end, out Dec_end);
					Utilities.TopocentricPosition(JD_atEvent + 1.1574074074074073E-05, RA, Dec, 4.26352124542639E-05 / Delta, Longitude, Latitude, Height, out RA_Planet, out Dec_Planet);
					text = "JPLhor";
				}
			}
			else if (StarCat == "A")
			{
				http.GetHorizons_Asteroid_or_Satellite_ApparentPosition(GetAsteroid: true, num8, 0, 0, JD_atEvent, UseTTnotUT: false, out var RA2, out var Dec2, out Delta, out SatName, out SatDia, out var _, out var _);
				Utilities.TopocentricPosition(JD_atEvent, RA2, Dec2, 4.26352124542639E-05 / Delta, Longitude, Latitude, Height, out RA_end, out Dec_end);
				Utilities.TopocentricPosition(JD_atEvent + 1.1574074074074073E-05, RA2, Dec2, 4.26352124542639E-05 / Delta, Longitude, Latitude, Height, out RA_Planet, out Dec_Planet);
				text = "JPLhor";
			}
			Utilities.TopocentricMoon(JD_atEvent + 1.1574074074074073E-05, Longitude, Latitude, Height, Height_is_MSL, DatumNumber, out var RA3, out var Dec3, out MoonRadius_Radians, out var _, out var _, out var _, out var _, out var MoonAlt_Deg);
			Utilities.TopocentricMoon(JD_atEvent, Longitude, Latitude, Height, Height_is_MSL, DatumNumber, out var RA4, out var Dec4, out MoonRadius, out var MoonScale2, out var l2, out var b2, out var C2, out MoonAlt_Deg);
			MoonAlt = Utilities.Altitude(JD_atEvent, RA4, Dec4, Longitude, Latitude);
			Utilities.QuickPlanet(JD_atEvent, 3, EquinoxOfDate: true, out var RA5, out var Dec5, out MoonAlt_Deg);
			SunAlt = Utilities.Altitude(JD_atEvent, RA5, Dec5, Longitude, Latitude);
			Utilities.Distance(RA5, Dec5, RA4, Dec4, out var Distance2, out MoonAlt_Deg);
			Illumination = 50.0 * (1.0 - Math.Cos(Distance2)) * Math.Sin(RA4 - RA5);
			Utilities.Distance(RA3, Dec3, RA_Planet, Dec_Planet, out Distance, out var PA_atOrigin);
			Utilities.Distance(RA4, Dec4, RA_end, Dec_end, out var Distance3, out PA);
			Utilities.Distance(RA4, Dec4, RA3, Dec3, out var Distance4, out var PA_atOrigin2);
			PA_atOrigin2 *= 180.0 / Math.PI;
			if (PA_atOrigin2 < 0.0)
			{
				PA_atOrigin2 += 360.0;
			}
			Distance4 *= 648000.0 / Math.PI;
			PA *= 180.0 / Math.PI;
			PA_atOrigin *= 180.0 / Math.PI;
			if (flag)
			{
				if (Utilities.LOLAFileExists)
				{
					num = LOLAHiRes.LimbHeight_Slope(PA - C2, l2, b2, 1.0, IncludeSlope: false, WideSlope: false, out var SlopeBefore_Deg, out var SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
					num5 = LOLAHiRes.LimbHeight_Slope(PA_atOrigin - C2, l2, b2, 1.0, IncludeSlope: false, WideSlope: false, out SlopeBefore_Deg, out SlopeAfter_Deg, out P_Limb_deg, out D_Limb_deg);
				}
				else
				{
					num = (num5 = 0.0);
				}
			}
			Residual = (Distance3 - MoonRadius) * (648000.0 / Math.PI);
			double num9 = (Distance - MoonRadius) * (648000.0 / Math.PI);
			if (ApplyLimbCorrn)
			{
				Residual -= MoonScale2 * num;
				num9 -= MoonScale2 * num5;
			}
			if (StarCat == "P")
			{
				if (num7 == 0)
				{
					Residual += PlanetRadius;
					num9 += PlanetRadius;
				}
				else if (UseOuterLimbOfSatelliteOrAsteroid)
				{
					Residual += SatDia;
					num9 += (double)SatDia;
				}
			}
			if (Math.Abs(Residual) > 9999.0)
			{
				num9 = (Residual = -9999.0);
			}
			if (PA < 0.0)
			{
				PA += 360.0;
			}
			double num10 = PA - C2;
			if (num10 < 0.0)
			{
				num10 += 360.0;
			}
			if (num10 >= 360.0)
			{
				num10 -= 360.0;
			}
			if (VizierReduction)
			{
				Reduction = string.Format("{0,8:F2}", l2) + string.Format("{0,6:F2}", b2) + string.Format("{0,7:F2}", num10) + string.Format("{0,6:F3}", MoonScale2) + string.Format("{0,7:F3} ", MoonScale2 * num) + string.Format("{0,9:+0.000;-0.000}", Residual).Substring(0, 9) + string.Format("{0,7:F2}", PA) + string.Format("{0,7:F2}", num9 - Residual) + string.Format("{0,6:F2}", Distance4) + string.Format("{0,7:F2}", PA_atOrigin2) + " " + text.PadRight(6).Substring(0, 6);
			}
			else
			{
				Utilities.Librations_P_D(l2, b2, num10, out var P_deg, out var D_deg);
				Reduction = string.Format("{0,8:+0.000;-0.000}", Residual).Substring(0, 8) + string.Format("{0,7:F2}", PA) + string.Format("{0,8:F2}", l2) + string.Format("{0,6:F2}", b2) + string.Format("{0,7:F2}", num10) + string.Format("{0,7:F3}", MoonScale2 * num) + string.Format("{0,6:F3}", MoonScale2) + string.Format("{0,7:F2}", P_deg) + string.Format("{0,6:F2}", D_deg) + string.Format("{0,7:F2}", num9 - Residual) + string.Format("{0,6:F2}", Distance4) + string.Format("{0,7:F2}", PA_atOrigin2) + " " + text.PadRight(6).Substring(0, 6);
			}
			return flag;
		}

		internal static void Reduce_Displayed_File()
		{
			double P_Limb_deg = 0.0;
			double D_Limb_deg = 0.0;
			double num = 0.0;
			double num2 = 0.0;
			double RA_Planet = 0.0;
			double Dec_Planet = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			string text = "";
			string positionSource = "";
			int num5 = 0;
			int num6 = -1;
			bool inGaia = false;
			bool flag = false;
			int count = OccMain.Events.Count;
			float[] array = new float[count + 1];
			int num7 = 0;
			int num8 = 0;
			float[] array2 = new float[count + 1];
			Residuals.Clear();
			PBar pBar = new PBar();
			((Control)pBar).set_Text("Reducing observations - " + CurrentSourceFile);
			pBar.pBarFTP.set_Maximum(count);
			pBar.pBarFTP.set_Minimum(0);
			((Control)pBar).Show();
			for (int i = 0; i < count; i++)
			{
				if (i % 5 == 0)
				{
					pBar.pBarFTP.set_Value(i);
					Application.DoEvents();
				}
				Reduction_Line = new ReductionLine();
				Reduction_Line.SeqNumber = OccMain.Events[i].SeqNumber;
				Reduction_Line.Observer = "***";
				string eventObserver = OccMain.Events[i].EventObserver;
				for (int j = 0; j < OccMain.Observers.Count; j++)
				{
					if (OccMain.Observers[j].ObserverCodeForEvent == eventObserver)
					{
						Reduction_Line.Observer = OccMain.Observers[j].ObserverName;
						break;
					}
				}
				eventObserver = OccMain.Events[i].EventTelescope;
				for (int k = 0; k < OccMain.Telescopes.Count; k++)
				{
					if (OccMain.Telescopes[k].TelescopeCodeForEvent == eventObserver)
					{
						num6 = k;
						break;
					}
				}
				if (num6 >= 0)
				{
					double num9 = OccMain.Events[i].EventJDTime_PEcorrected;
					flag = OccMain.Events[i].PE_AdjustIfBrightStar;
					double longitude = OccMain.Telescopes[num6].Longitude;
					double latitude = OccMain.Telescopes[num6].Latitude;
					double altitude = OccMain.Telescopes[num6].Altitude;
					bool altitude_is_MSL = OccMain.Telescopes[num6].VerticalDatum == "M";
					int num10 = OccMain.Telescopes[num6].DatumNumber;
					EventCode = OccMain.Events[i].Phase_Old;
					text = OccMain.Events[i].StarCat;
					int starNumber = OccMain.Events[i].StarNumber;
					Utilities.EarthOrientationParameters(num9, out var x, out var y, out var _);
					longitude += (x * Math.Cos(longitude) - y * Math.Sin(longitude)) / 3600.0 / (180.0 / Math.PI);
					latitude += (x * Math.Sin(longitude) + y * Math.Cos(longitude)) * Math.Tan(latitude) / 3600.0 / (180.0 / Math.PI);
					if (num10 > 0 && num10 < 6)
					{
						num10 = Utilities.RGOdatum_to_ILOCdatum(num10, longitude * (180.0 / Math.PI), latitude * (180.0 / Math.PI));
					}
					bool flag2 = false;
					int num11;
					int num12 = (num11 = (num5 = 0));
					num4 = 0.0;
					switch (text)
					{
					case "R":
						if (starNumber > 4000 && starNumber < 4010)
						{
							num12 = starNumber - 4000;
							flag2 = true;
						}
						else if (starNumber > 4050 && starNumber < 4055)
						{
							num12 = 5;
							num11 = starNumber - 4050;
							flag2 = true;
						}
						else if (starNumber > 4060 && starNumber < 4069)
						{
							num12 = 6;
							num11 = starNumber - 4060;
							flag2 = true;
						}
						else
						{
							flag2 = XZ80Q.Get_ZC_Star(starNumber);
						}
						break;
					case "S":
						flag2 = XZ80Q.Get_SAO_Star(starNumber);
						break;
					case "X":
						flag2 = XZ80Q.Get_XZ_Star(starNumber);
						break;
					case "P":
						num12 = starNumber / 1000;
						num11 = starNumber % 100;
						flag2 = true;
						break;
					case "A":
						num5 = starNumber;
						flag2 = true;
						break;
					}
					Reduction_Line.StarCat = text;
					Reduction_Line.StarNumber = starNumber;
					string SatName = (Reduction_Line.WDSCode = OccMain.Events[i].WDS);
					string text2 = SatName;
					Reduction_Line.SiteCode = OccMain.Events[i].EventTelescope;
					Reduction_Line.Year = OccMain.Events[i].Year;
					Reduction_Line.Month = OccMain.Events[i].Month;
					Reduction_Line.Day = OccMain.Events[i].Day;
					Reduction_Line.Hour = OccMain.Events[i].Hour;
					Reduction_Line.Minute = OccMain.Events[i].Minute;
					Reduction_Line.Second = OccMain.Events[i].Second;
					Reduction_Line.Second_DecPlaces = OccMain.Events[i].Second_DecPlaces;
					Reduction_Line.EventPhase = OccMain.Events[i].OccEvent;
					Reduction_Line.EventLimb = OccMain.Events[i].Limb;
					Reduction_Line.DoubleCode = OccMain.Events[i].DoubleStarComponent;
					Reduction_Line.Certainty = OccMain.Events[i].Certainty;
					Reduction_Line.GrazeFlag = OccMain.Events[i].GrazeFlag;
					Reduction_Line.MethodTimeRecording = OccMain.Events[i].MethodRecording1;
					Reduction_Line.DoubleStarComponent = OccMain.Events[i].DoubleStarComponent;
					if (flag2)
					{
						if (num5 > 0)
						{
							http.GetHorizons_Asteroid_or_Satellite_ApparentPosition(GetAsteroid: true, num5, 0, 0, num9, UseTTnotUT: false, out var RA, out var Dec, out var Delta, out SatName, out var _, out var _, out var _);
							Utilities.TopocentricPosition(num9, RA, Dec, 4.26352124542639E-05 / Delta, longitude, latitude, altitude, out RA_Planet, out Dec_Planet);
							positionSource = "JPLhor";
						}
						else if (num12 == 0)
						{
							inGaia = XZ80Q.GetGaiaData(XZ80Q.DoubleFlag);
							num = XZ80Q.RA_rad;
							num2 = XZ80Q.Dec_rad;
							double pMRA_rad = XZ80Q.PMRA_rad;
							double pMDec_rad = XZ80Q.PMDec_rad;
							positionSource = XZ80Q.PositionSource;
							_ = XZ80Q.Mv;
							num3 = XZ80Q.Parallax_Rad;
							if (XZ80Q.Mv <= 4.0 && flag)
							{
								num9 += 2.3148148148148148E-06;
							}
							if ((XZ80Q.DoubleFlag != " ") & (text2 == " "))
							{
								text2 = DoubleStars.ComponentIfMeanPosition(XZ80Q.XZ);
							}
							if (text2 != " ")
							{
								Get_DoubleStar_Sep_PA(XZ80Q.XZ, num9, text2, out var ComponentSep, out var ComponentPA);
								num += ComponentSep * Math.Sin(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI) / Math.Cos(Dec_Planet);
								num2 += ComponentSep * Math.Cos(ComponentPA / (180.0 / Math.PI)) / 3600.0 / (180.0 / Math.PI);
							}
							Utilities.ApparentStarPosition(num, num2, pMRA_rad, pMDec_rad, num3, 2000, IncludeRelativisticBending: true, num9, use2006values_Not1976: true, out RA_Planet, out Dec_Planet);
						}
						else
						{
							Utilities.TopocentricPlanet(num9, num12, longitude, latitude, altitude, out RA_Planet, out Dec_Planet, out num4);
							if (num11 > 0)
							{
								float MoonDiaKm = 0f;
								float Mag = 0f;
								string MoonName = "";
								double dRA = 0.0;
								double dDec = 0.0;
								Satellites.SatelliteCoordinates(num9, num12, num11, ViewedFromSun: false, ref MoonName, ref MoonDiaKm, ref dRA, ref dDec, ref Mag);
								RA_Planet += dRA;
								Dec_Planet += dDec;
							}
							else
							{
								positionSource = Utilities.EphemerisBasis().Substring(0, 6);
							}
						}
						Utilities.TopocentricMoon(num9, longitude, latitude, altitude, altitude_is_MSL, num10, out var RA2, out var Dec2, out var MoonRadius_Radians, out var MoonScale, out var l, out var b, out var C, out var MoonAlt_Deg);
						Utilities.Distance(RA2, Dec2, RA_Planet, Dec_Planet, out var Distance, out var PA_atOrigin);
						PA_atOrigin *= 180.0 / Math.PI;
						if (PA_atOrigin < 0.0)
						{
							PA_atOrigin += 360.0;
						}
						double num13 = ((!Utilities.LOLAFileExists) ? 0.0 : LOLAHiRes.LimbHeight_Slope(PA_atOrigin - C, l, b, 1.0, IncludeSlope: false, WideSlope: false, out var _, out var _, out P_Limb_deg, out D_Limb_deg));
						double num14 = MoonScale * num13;
						double num15 = (Distance - MoonRadius_Radians) * (180.0 / Math.PI) * 3600.0 - num14;
						if (num12 > 0 && num11 == 0)
						{
							num15 += num4 * (180.0 / Math.PI) * 3600.0;
						}
						Reduction_Line.O_C = num15;
						Reduction_Line.InGaia = inGaia;
						Reduction_Line.Limb = num14;
						Reduction_Line.PA = PA_atOrigin;
						Reduction_Line.l = l;
						Reduction_Line.b = b;
						Reduction_Line.AA = PA_atOrigin - C;
						Utilities.Librations_P_D(l, b, PA_atOrigin - C, out var P_deg, out var D_deg);
						Reduction_Line.P = P_deg;
						Reduction_Line.D = D_deg;
						Reduction_Line.Scale = MoonScale;
						Reduction_Line.PositionSource = positionSource;
						Utilities.TopocentricMoon(num9 + 1.1574074074074073E-05, longitude, latitude, altitude, altitude_is_MSL, num10, out var RA3, out var Dec3, out var MoonRadius_Radians2, out MoonAlt_Deg, out var l2, out var b2, out var C2, out var MoonAlt_Deg2);
						Utilities.Distance(RA3, Dec3, RA_Planet, Dec_Planet, out var Distance2, out var PA_atOrigin2);
						PA_atOrigin2 *= 180.0 / Math.PI;
						if (PA_atOrigin2 < 0.0)
						{
							PA_atOrigin2 += 360.0;
						}
						num13 = ((!Utilities.LOLAFileExists) ? 0.0 : LOLAHiRes.LimbHeight_Slope(PA_atOrigin2 - C, l, b, 1.0, IncludeSlope: false, WideSlope: false, out MoonAlt_Deg2, out C2, out b2, out l2));
						double num16 = MoonScale * num13;
						double num17 = (Distance2 - MoonRadius_Radians2) * (180.0 / Math.PI) * 3600.0 - num16;
						if (num12 > 0 && num11 == 0)
						{
							num17 += num4 * (180.0 / Math.PI) * 3600.0;
						}
						Reduction_Line.ResidualRate = num17 - num15;
						if ((num12 == 0 && num5 == 0) & (Reduction_Line.DoubleCode.Trim() == "") & ("SEM".IndexOf(Reduction_Line.EventPhase) < 0) & !"B".Contains(Reduction_Line.EventLimb) & ((Reduction_Line.Year > 1900) & (Math.Abs(num15) < 0.5)) & (Reduction_Line.Certainty == 1))
						{
							array[num7] += (float)num15;
							num7++;
							if (Math.Abs(Reduction_Line.ResidualRate) > 0.15)
							{
								array2[num8] += 0f - (float)(num15 / Reduction_Line.ResidualRate);
								num8++;
							}
						}
					}
				}
				Residuals.Add(Reduction_Line);
			}
			((Form)pBar).Close();
			((Component)(object)pBar).Dispose();
			float num18 = 0f;
			float num19 = 0f;
			float num20 = 0f;
			float num21 = 0f;
			for (int m = 0; m < num7; m++)
			{
				num18 += array[m];
			}
			num18 /= (float)num7;
			for (int n = 0; n < num8; n++)
			{
				num20 += array2[n];
			}
			num20 /= (float)num8;
			for (int num22 = 0; num22 < num7; num22++)
			{
				num19 += (num18 - array[num22]) * (num18 - array[num22]);
			}
			if (num7 > 3)
			{
				num19 = (float)Math.Sqrt(num19 / (float)num7);
			}
			for (int num23 = 0; num23 < num8; num23++)
			{
				num21 += (num20 - array2[num23]) * (num20 - array2[num23]);
			}
			if (num8 > 3)
			{
				num21 = (float)Math.Sqrt(num21 / (float)num8);
			}
			string text3 = string.Format("Mean residual of {0,1:f0} events involving single stars: {1:+0;-0} mas", num7, num18 * 1000f);
			if (num7 > 3)
			{
				text3 += string.Format(" ±{0,1:#} mas", num19 * 1000f);
			}
			string text4 = "";
			if (num8 > 0)
			{
				text4 += string.Format("Mean clock correction from {0,1:f0} event times: {1:+0.00;-0.00} secs", num8, num20);
				if (num8 > 3)
				{
					text4 += string.Format(" ±{0,4:f2} secs", num21);
				}
			}
			Show_ListResiduals();
			List_Residuals.CorrectionApplied = true;
			List_Residuals.SourceFile = CurrentSourceFile;
			List_Residuals.Mean_SD_Text = text3;
			List_Residuals.Mean_SDSecs = text4;
			List_Residuals.DisplayResiduals();
			((Control)List_Residuals).Focus();
		}

		internal static void GoogleEarthMoonOutline_DisplayedFile(int NumberOfLines, int Height, bool IncludeLimb)
		{
			//IL_006b: Unknown result type (might be due to invalid IL or missing references)
			double scale = 1.0;
			double num = 0.2;
			bool flag = false;
			bool flag2 = NumberOfLines <= 0;
			bool flag3 = false;
			double num2 = 1.0 + (double)Height / 6378.137 / 1000.0;
			if (OccMain.Events.Count < 1)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			GoogleEarth.Create_New_GoogleEarthKMZ_File(AppPath + "\\Observations\\MultiEvent Moon Limb Plots.kml", "Limb plots for occultations", AutoOpenFile: true, out var CreatedFile);
			Cursor.set_Current(Cursors.get_WaitCursor());
			for (int i = 0; i < OccMain.Events.Count; i++)
			{
				flag = false;
				if (!flag2 && i > NumberOfLines)
				{
					break;
				}
				string starCat = OccMain.Events[i].StarCat;
				if (!"RSX".Contains(starCat))
				{
					continue;
				}
				double num3 = OccMain.Events[i].EventJDTime_PEcorrected;
				flag3 = OccMain.Events[i].PE_AdjustIfBrightStar;
				int starNumber = OccMain.Events[i].StarNumber;
				bool flag4 = false;
				switch (starCat)
				{
				case "R":
					flag4 = XZ80Q.Get_ZC_Star(starNumber);
					break;
				case "S":
					flag4 = XZ80Q.Get_SAO_Star(starNumber);
					break;
				case "X":
					flag4 = XZ80Q.Get_XZ_Star(starNumber);
					break;
				}
				if (!flag4)
				{
					continue;
				}
				string label = Utilities.DateTime_from_JD(OccMain.Events[i].EventJDTime);
				double RA = XZ80Q.RA_rad;
				double Dec = XZ80Q.Dec_rad;
				if (XZ80Q.Mv <= 4.0 && flag3)
				{
					num3 += 2.3148148148148148E-06;
				}
				Utilities.ApparentStarPosition(ref RA, ref Dec, XZ80Q.PMRA_rad, XZ80Q.PMDec_rad, 2000, num3, use2006values_Not1976: false);
				double num4 = Utilities.SiderealTime_deg(num3, Apparent: true) / (180.0 / Math.PI) - RA;
				double num5 = Math.Sqrt(1.0 - 0.006694385 * Math.Cos(Dec) * Math.Cos(Dec));
				double num6 = Math.Sin(Dec) / num5;
				double num7 = Math.Cos(Dec) * Math.Sqrt(0.993305615) / num5;
				Utilities.GeocentricMoon(num3, out var RA2, out var Dec2, out var PiMoon, out var l, out var b, out var C);
				double num8 = Math.Cos(Dec2) * Math.Sin(RA2 - RA) / Math.Sin(PiMoon);
				double num9 = (Math.Sin(Dec2) * Math.Cos(Dec) - Math.Cos(Dec2) * Math.Sin(Dec) * Math.Cos(RA2 - RA)) / Math.Sin(PiMoon);
				double num10 = 0.0;
				do
				{
					double num11 = 0.0;
					if (IncludeLimb)
					{
						num = 0.2;
						if (Utilities.LOLAFileExists)
						{
							num11 = Utilities.LOLA_LimbHeight_ActualDistance(num10 - C, l, b, scale);
							num = 0.05;
						}
						else
						{
							num11 = 0.0;
						}
						num11 = num11 * 1.864 / 6378.137;
					}
					double num12 = num8 + (0.27246972144360004 + num11) * Math.Sin(num10 / (180.0 / Math.PI));
					double num13 = num9 + (0.27246972144360004 + num11) * Math.Cos(num10 / (180.0 / Math.PI));
					num12 /= num2;
					double num14 = num13 / num2 / num5;
					double num15 = 1.0 - num12 * num12 - num14 * num14;
					if (num15 < 0.0)
					{
						double num16 = 0.0;
						if (flag)
						{
							GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
						}
						flag = false;
					}
					else
					{
						double num16 = Math.Sqrt(num15);
						double num17 = num12;
						double num18 = (0.0 - num14) * num6 + num16 * num7;
						double num19 = num14 * num7 + num16 * num6;
						double num20;
						for (num20 = (Math.Atan2(num17, num18) - num4) * (180.0 / Math.PI); num20 > 180.0; num20 -= 360.0)
						{
						}
						for (; num20 < -180.0; num20 += 360.0)
						{
						}
						double latitude = 180.0 / Math.PI * Math.Atan(num19 / Math.Sqrt(num17 * num17 + num18 * num18) / Utilities.sqrt_1LessEarthEllipticitySqrd);
						if (!flag)
						{
							GoogleEarth.Write_TagsAndLabel_For_New_GoogleEarthKMZ_Path(i % 18, label);
						}
						GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(num20, latitude, 0.0);
						flag = true;
					}
					num10 += num;
				}
				while (num10 < 360.0);
				if (flag)
				{
					GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			GoogleEarth.DisplayGoogleMap(CreatedFile);
		}

		internal static void Get_DoubleStar_Sep_PA(int XZ, double JD, string WDScode, out double ComponentSep, out double ComponentPA)
		{
			string text = "";
			string[] array = new string[4] { "", "", "", "" };
			bool[] isMean = new bool[4];
			double[] posnAngle = new double[4];
			double[] separation = new double[4];
			DoubleStars.GetXZDoubleMatches(XZ80Q.XZ, JD, ForReductions: true, out var _, array, posnAngle, separation, isMean, out var _, out var _, out var _, out var _);
			ComponentSep = (ComponentPA = 0.0);
			for (int i = 0; i < 4; i++)
			{
				if (array[i].Substring(0, 6).Trim().EndsWith(WDScode))
				{
					text = array[i];
					break;
				}
				if (array[i].Substring(0, 6).Trim().StartsWith(WDScode))
				{
					text = array[i];
					break;
				}
			}
			if (text.Length <= 14)
			{
				return;
			}
			if (!double.TryParse(text.Substring(6, 8), out ComponentSep))
			{
				ComponentSep = 0.0;
			}
			if (!double.TryParse(text.Substring(14, 7), out ComponentPA))
			{
				ComponentPA = 0.0;
			}
			if (!double.TryParse(text.Substring(21, 7), out var result))
			{
				result = 0.0;
			}
			if (text.Substring(0, 6).Trim().StartsWith(WDScode))
			{
				result = 1.0 - result;
				ComponentPA += 180.0;
				if (ComponentPA >= 360.0)
				{
					ComponentPA -= 360.0;
				}
			}
			ComponentSep *= result;
		}

		private static void Sort_ListedResiduals(int SortField)
		{
			ReductionLine.SortField = SortField;
			Residuals.Sort();
		}

		internal static void ShowGrazeFileEventInEditor(int First, int Last, bool IncludeInvalid, bool SourceUsesOldFormat)
		{
			string text = "";
			string text2 = "     ";
			string text3 = "";
			if (!SourceUsesOldFormat)
			{
				for (int i = First; i < Last; i++)
				{
					if (HistoricalGrazes.lstEvents.get_Items().get_Item(i).ToString()!.PadRight(30).Substring(0, 12) == "Event ID  : ")
					{
						text3 = HistoricalGrazes.lstEvents.get_Items().get_Item(i).ToString()!.PadRight(30).Substring(12, 7);
						break;
					}
				}
				if (text3.Length != 7)
				{
					return;
				}
			}
			Show_ObservationsEditor();
			OccMain.Events.Clear();
			OccMain.Observers.Clear();
			OccMain.Telescopes.Clear();
			OccMain.Place = "Graze imported from archive files";
			OccMain.EMail = "";
			OccMain.Address = "";
			OccMain.Representative = "";
			OccMain.Reported = "";
			EventLine.UseOldFormat = SourceUsesOldFormat;
			TelescopeLine.UseOldFormat = SourceUsesOldFormat;
			ObserverLine.UseOldFormat = SourceUsesOldFormat;
			if (SourceUsesOldFormat)
			{
				text2 = "     ";
				string text4 = HistoricalGrazes.lstEvents.get_Items().get_Item(First).ToString()!.PadRight(30);
				for (int j = First + 1; j <= Last; j++)
				{
					text = text4;
					text4 = HistoricalGrazes.lstEvents.get_Items().get_Item(j).ToString()!.PadRight(30);
					text2 = OccMain.DecodeLine(text, text2, j == Last);
				}
			}
			else
			{
				new ArrayList();
				ArrayList arrayList = new ArrayList();
				ArrayList arrayList2 = new ArrayList();
				string text5 = "";
				string text6 = "";
				string text7 = "";
				string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
				foreach (string text8 in files)
				{
					if (text8.Contains(".zip"))
					{
						continue;
					}
					using StreamReader streamReader = new StreamReader(text8);
					do
					{
						text = streamReader.ReadLine();
						if ((!IncludeInvalid && "WXZ".Contains(text.Substring(73, 1))) || !(text.Substring(74, 7) == text3) || (!IncludeInvalid && ("WXZ".Contains(text.Substring(73, 1)) || "3".Contains(text.Substring(42, 1)))))
						{
							continue;
						}
						text6 = "O   " + text.Substring(188, 25).Trim();
						text5 = "T   " + text.Substring(177, 3) + " " + text.Substring(180, 4) + "  " + text.Substring(184, 4) + "  " + text.Substring(81, 11) + " " + text.Substring(92, 10) + " " + text.Substring(102, 2) + " " + text.Substring(104, 7);
						text7 = "";
						for (int l = 0; l < arrayList.Count; l++)
						{
							if (arrayList[l]!.ToString()!.Substring(2) == text5.Substring(2))
							{
								text7 = arrayList[l]!.ToString()!.Substring(1, 1);
								break;
							}
						}
						if (text7 == "")
						{
							int num = 65 + arrayList.Count;
							if (num > 90)
							{
								num += 6;
							}
							if (num > 126)
							{
								num += 65;
							}
							text7 = Convert.ToString((char)num);
							if (text6 == "O   ")
							{
								text6 = "O   Unknown " + text7;
							}
							text6 = text6.Remove(1, 1).Insert(1, text7);
							arrayList2.Add(text6);
							OccMain.DecodeLine(text6.PadRight(6), "   ", AtEndOfData: false);
							text5 = text5.Remove(1, 1).Insert(1, text7);
							arrayList.Add(text5);
							OccMain.DecodeLine(text5.PadRight(6), "   ", AtEndOfData: false);
						}
						OccMain.DecodeLine(RGOtoILOC(text.Substring(0, 63)) + text7 + text7, "   ", AtEndOfData: true);
					}
					while (!streamReader.EndOfStream);
				}
			}
			EventLine.SortField = 1;
			OccMain.Events.Sort();
			OccMain.ReNumberEvents();
			Observations_Editor.DisplayReport(AllForms: true, HighlightLast: false);
		}

		internal static void ShowGrazesInArchiveEditorInEditor(bool ExcludeStartEnd, bool ExcludeBad)
		{
			string text = "";
			Show_ObservationsEditor();
			OccMain.Events.Clear();
			OccMain.Observers.Clear();
			OccMain.Telescopes.Clear();
			OccMain.Place = "Graze imported from Archive Editor";
			OccMain.EMail = "";
			OccMain.Address = "";
			OccMain.Representative = "";
			OccMain.Reported = "";
			EventLine.UseOldFormat = false;
			TelescopeLine.UseOldFormat = false;
			ObserverLine.UseOldFormat = false;
			new ArrayList();
			ArrayList arrayList = new ArrayList();
			ArrayList arrayList2 = new ArrayList();
			string text2 = "";
			string text3 = "";
			string text4 = "";
			for (int i = 0; i < ((ObjectCollection)Archive_Editor.lstCheckedEdit.get_Items()).get_Count(); i++)
			{
				text = ((ObjectCollection)Archive_Editor.lstCheckedEdit.get_Items()).get_Item(i).ToString();
				if (!(text.Substring(28, 1) == "G") || (ExcludeStartEnd && "MSEO ".IndexOf(text.Substring(26, 1)) >= 0) || (ExcludeBad && "DIK".IndexOf(text.Substring(72, 1)) >= 0))
				{
					continue;
				}
				text3 = "O   " + text.Substring(188, 25).Trim();
				text2 = "T   " + text.Substring(177, 3) + " " + text.Substring(180, 4) + "  " + text.Substring(184, 4) + "  " + text.Substring(81, 11) + " " + text.Substring(92, 10) + " " + text.Substring(102, 2) + " " + text.Substring(104, 7);
				text4 = "";
				for (int j = 0; j < arrayList.Count; j++)
				{
					if (arrayList[j]!.ToString()!.Substring(2) == text2.Substring(2))
					{
						text4 = arrayList[j]!.ToString()!.Substring(1, 1);
						break;
					}
				}
				if (text4 == "")
				{
					int num = 65 + arrayList.Count;
					if (num > 90)
					{
						num += 6;
					}
					if (num > 126)
					{
						num += 65;
					}
					text4 = Convert.ToString((char)num);
					if (text3 == "O   ")
					{
						text3 = "O   Unknown " + text4;
					}
					if (text3 == "O   ?")
					{
						text3 = "O   Unknown " + text4;
					}
					text3 = text3.Remove(1, 1).Insert(1, text4);
					arrayList2.Add(text3);
					OccMain.DecodeLine(text3.PadRight(6), "   ", AtEndOfData: false);
					text2 = text2.Remove(1, 1).Insert(1, text4);
					arrayList.Add(text2);
					OccMain.DecodeLine(text2.PadRight(6), "   ", AtEndOfData: false);
				}
				OccMain.DecodeLine(RGOtoILOC(text.Substring(0, 63)) + text4 + text4, "   ", AtEndOfData: true);
			}
			EventLine.SortField = 1;
			OccMain.Events.Sort();
			OccMain.ReNumberEvents();
			Observations_Editor.DisplayReport(AllForms: true, HighlightLast: false);
			Observations_Editor.Reduce();
			ReductionProfile.Show_ReductionProfile(-1);
		}

		internal static void MergeGrazeFileEventInEditor(int First, int Last, bool IncludeInvalid, bool UseOldFormat, bool SourceUsesOldFormat)
		{
			string text = "";
			string text2 = "     ";
			string text3 = "";
			if (!SourceUsesOldFormat)
			{
				for (int i = First; i < Last; i++)
				{
					if (HistoricalGrazes.lstEvents.get_Items().get_Item(i).ToString()!.PadRight(30).Substring(0, 12) == "Event ID  : ")
					{
						text3 = HistoricalGrazes.lstEvents.get_Items().get_Item(i).ToString()!.PadRight(30).Substring(12, 7);
						break;
					}
				}
				if (text3.Length != 7)
				{
					return;
				}
			}
			EventLine.UseOldFormat = SourceUsesOldFormat;
			TelescopeLine.UseOldFormat = SourceUsesOldFormat;
			ObserverLine.UseOldFormat = SourceUsesOldFormat;
			Show_ObservationsEditor();
			OccMerge.Events.Clear();
			OccMerge.Observers.Clear();
			OccMerge.Telescopes.Clear();
			if (SourceUsesOldFormat)
			{
				text2 = "     ";
				string text4 = HistoricalGrazes.lstEvents.get_Items().get_Item(First).ToString()!.PadRight(30);
				for (int j = First + 1; j <= Last; j++)
				{
					text = text4;
					text4 = HistoricalGrazes.lstEvents.get_Items().get_Item(j).ToString()!.PadRight(30);
					text2 = OccMerge.DecodeLine(text, text2, j == Last);
				}
			}
			else
			{
				new ArrayList();
				ArrayList arrayList = new ArrayList();
				ArrayList arrayList2 = new ArrayList();
				string text5 = "";
				string text6 = "";
				string text7 = "";
				string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
				for (int k = 0; k < files.Length; k++)
				{
					using StreamReader streamReader = new StreamReader(files[k]);
					do
					{
						text = streamReader.ReadLine();
						if ((!IncludeInvalid && ("WXZ".Contains(text.Substring(73, 1)) || " 3".Contains(text.Substring(42, 1)))) || !(text.Substring(74, 7) == text3))
						{
							continue;
						}
						text6 = "O   " + text.Substring(188, 25).Trim();
						text5 = "T   " + text.Substring(177, 3) + " " + text.Substring(180, 4) + "  " + text.Substring(184, 4) + "  " + text.Substring(81, 11) + " " + text.Substring(92, 10) + " " + text.Substring(102, 2) + " " + text.Substring(104, 7);
						text7 = "";
						for (int l = 0; l < arrayList.Count; l++)
						{
							if (arrayList[l]!.ToString()!.Substring(2) == text5.Substring(2))
							{
								text7 = arrayList[l]!.ToString()!.Substring(1, 1);
								break;
							}
						}
						if (text7 == "")
						{
							int num = 65 + arrayList.Count;
							if (num > 90)
							{
								num += 6;
							}
							if (num > 126)
							{
								num += 65;
							}
							text7 = Convert.ToString((char)num);
							if (text6 == "O   ")
							{
								text6 = "O   Unknown " + text7;
							}
							text6 = text6.Remove(1, 1).Insert(1, text7);
							arrayList2.Add(text6);
							OccMerge.DecodeLine(text6.PadRight(6), "   ", AtEndOfData: false);
							text5 = text5.Remove(1, 1).Insert(1, text7);
							arrayList.Add(text5);
							OccMerge.DecodeLine(text5.PadRight(6), "   ", AtEndOfData: false);
						}
						OccMerge.DecodeLine(RGOtoILOC(text.Substring(0, 63)) + text7 + text7, "   ", AtEndOfData: true);
					}
					while (!streamReader.EndOfStream);
				}
			}
			OccMain.ReNumberSites("A");
			OccMain.ReNumberNames("A");
			OccMerge.ReNumberSites(OccMain.GetNextSiteID());
			OccMerge.ReNumberNames(OccMain.GetNextNameID());
			for (int m = 0; m < OccMerge.Telescopes.Count; m++)
			{
				OccMain.AddNewTelescopeLine(OccMerge.Telescopes[m].ToString());
			}
			for (int n = 0; n < OccMerge.Observers.Count; n++)
			{
				OccMain.AddNewObserverLine(OccMerge.Observers[n].ToString());
			}
			for (int num2 = 0; num2 < OccMerge.Events.Count; num2++)
			{
				OccMerge.Events[num2].GetEventLines(out var Line, out var Line2);
				if (Line2.Trim().Length < 1)
				{
					Line2 = "";
				}
				OccMain.AddNewEventLine(Line, Line2);
			}
			EventLine.SortField = 1;
			OccMain.Events.Sort();
			OccMain.ReNumberEvents();
			Observations_Editor.DisplayReport(AllForms: true, HighlightLast: true);
		}

		internal static void SummaryGrazeList(int SummaryType)
		{
			bool flag = false;
			ArrayList arrayList = new ArrayList();
			Show_GrazeSummary();
			Graze_Summary.lstGrazes.get_Items().Clear();
			string[] array = new string[2] { "OccultGrazes.dat", "RecentGrazes.dat" };
			for (int i = 0; i <= 1; i++)
			{
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + array[i]);
				do
				{
					arrayList.Clear();
					int result;
					int result2;
					string text2;
					if (SummaryType != 0)
					{
						if (SummaryType == 1)
						{
							do
							{
								string text = streamReader.ReadLine()!.PadRight(2);
								text2 = text.Substring(0, 2);
								if (int.TryParse(text2, out result))
								{
									if (!int.TryParse(text.Substring(20, 7), out result2))
									{
										result2 = 0;
									}
									Graze_Summary.lstGrazes.get_Items().Add((object)(text.Substring(2, 4) + " " + text.Substring(6, 2) + " " + text.Substring(8, 2) + "     " + text.Substring(19, 1) + string.Format("{0,7}", result2)));
									break;
								}
							}
							while ((text2 != "**") & !streamReader.EndOfStream);
							do
							{
								string text = streamReader.ReadLine();
								text2 = text.Substring(0, 2);
							}
							while ((text2 != "**") & !streamReader.EndOfStream);
							continue;
						}
						do
						{
							string text = streamReader.ReadLine()!.PadRight(2);
							text2 = text.Substring(0, 2);
							if (int.TryParse(text2, out result))
							{
								string text3 = text.Substring(2, 4) + " " + Utilities.ShortMonths[int.Parse(text.Substring(6, 2))] + " " + text.Substring(8, 2);
								for (int j = 0; j < arrayList.Count; j++)
								{
									Graze_Summary.lstGrazes.get_Items().Add((object)(arrayList[j]!.ToString() + text3));
								}
								break;
							}
							if (text2.Substring(0, 1) == "O")
							{
								flag = false;
								if (((SummaryType == 2) | (SearchText.Length == 0)) || text.Substring(2).PadRight(25).Substring(0, 25)
									.ToUpper()
									.Contains(SearchText))
								{
									arrayList.Add(Utilities.ProperCase(text.Substring(2).PadRight(25).Substring(0, 25)
										.Replace(".", " ")
										.Replace("  ", " ")
										.TrimStart(Array.Empty<char>())
										.PadRight(25)
										.Substring(0, 25)));
								}
							}
						}
						while ((text2 != "**") & !streamReader.EndOfStream);
						do
						{
							string text = streamReader.ReadLine();
							text2 = text.Substring(0, 2);
						}
						while ((text2 != "**") & !streamReader.EndOfStream);
						continue;
					}
					do
					{
						string text = streamReader.ReadLine()!.PadRight(2);
						text2 = text.Substring(0, 2);
						if (int.TryParse(text2, out result))
						{
							if (!int.TryParse(text.Substring(20, 7), out result2))
							{
								result2 = 0;
							}
							Graze_Summary.lstGrazes.get_Items().Add((object)(text.Substring(19, 1) + string.Format("{0,7}", result2) + "  " + text.Substring(2, 4) + " " + Utilities.ShortMonths[int.Parse(text.Substring(6, 2))] + " " + text.Substring(8, 2)));
							break;
						}
					}
					while ((text2 != "**") & !streamReader.EndOfStream);
					do
					{
						string text = streamReader.ReadLine();
						text2 = text.Substring(0, 2);
					}
					while ((text2 != "**") & !streamReader.EndOfStream);
				}
				while (!streamReader.EndOfStream);
			}
		}

		public static void CreateGrazeIndexFile()
		{
			ArrayList arrayList = new ArrayList();
			ArrayList arrayList2 = new ArrayList();
			string[] array = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array2 = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array3 = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array4 = new string[8] { "", "", "", "", "", "", "", "" };
			int[] array5 = new int[8];
			string text = "";
			using StreamWriter streamWriter2 = new StreamWriter(AppPath + "\\Resource Files\\Archive Graze List.csv", append: false);
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files", "Archive Observations*.dat");
			for (int i = 0; i < files.Length; i++)
			{
				using StreamReader streamReader = new StreamReader(files[i]);
				do
				{
					string text2 = streamReader.ReadLine();
					string text3 = text2.Substring(74, 7);
					if (text3.Trim().Length <= 0)
					{
						continue;
					}
					text = text2.Substring(188, 25).Trim();
					bool flag = false;
					for (int num = 6; num >= 0; num--)
					{
						if (array[num] == text3)
						{
							if (text.Length > 0 && !array4[num].Contains(text))
							{
								ref string reference = ref array4[num];
								reference = reference + ", " + text;
							}
							array5[num]++;
							flag = true;
							break;
						}
					}
					if (!flag)
					{
						if (array[7].Length > 0)
						{
							arrayList.Add(array2[7] + "\r\n" + array[7].Replace(" ", "|") + "\r\n" + array3[7] + "\r\n" + array5[7] + "\r\n" + array4[7]);
							arrayList2.Add(array2[7] + "," + array[7] + "," + array3[7] + "," + array5[7]);
						}
						for (int num2 = 7; num2 > 0; num2--)
						{
							array[num2] = array[num2 - 1];
							array2[num2] = array2[num2 - 1];
							array3[num2] = array3[num2 - 1];
							array5[num2] = array5[num2 - 1];
							array4[num2] = array4[num2 - 1];
						}
						array[0] = text3;
						array2[0] = text2.Substring(0, 4) + " " + text2.Substring(4, 2) + " " + text2.Substring(6, 2);
						array3[0] = text2.Substring(18, 7) + text2.Substring(70, 1).Replace("2", "#");
						array5[0] = 1;
						array4[0] = text.PadRight(1);
					}
				}
				while (!streamReader.EndOfStream);
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\Archive Graze Index.dat", append: false);
			for (int num3 = 7; num3 >= 0; num3--)
			{
				if (array[num3].Length > 0)
				{
					arrayList.Add(array2[num3] + "\r\n" + array[num3].Replace(" ", "|") + "\r\n" + array3[num3] + "\r\n" + array5[num3] + "\r\n" + array4[num3]);
					arrayList2.Add(array2[num3] + "," + array[num3] + "," + array3[num3] + "," + array5[num3]);
				}
			}
			arrayList.Sort();
			arrayList2.Sort();
			for (int j = 0; j < arrayList.Count; j++)
			{
				streamWriter.WriteLine(arrayList[j]!.ToString()!.Replace("|", " "));
				streamWriter2.WriteLine(arrayList2[j]);
			}
		}

		public static void CreateSingleFileGrazeIndexFile()
		{
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0142: Expected O, but got Unknown
			//IL_0155: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Invalid comparison between Unknown and I4
			//IL_0185: Unknown result type (might be due to invalid IL or missing references)
			ArrayList arrayList = new ArrayList();
			string[] array = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array2 = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array3 = new string[8] { "", "", "", "", "", "", "", "" };
			string[] array4 = new string[8] { "", "", "", "", "", "", "", "" };
			int[] array5 = new int[8];
			string text = "";
			string text2 = "";
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_InitialDirectory(Settings.Default.Open_WorkingArchive);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			if (Path.GetFullPath(((FileDialog)val).get_FileName()).Contains(AppPath))
			{
				MessageBox.Show("This functionality cannot be run against files in the Occult 4 directory", "Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			text2 = Path.GetDirectoryName(((FileDialog)val).get_FileName());
			Settings.Default.Open_WorkingArchive = text2;
			using (StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName()))
			{
				do
				{
					string text3 = streamReader.ReadLine();
					string text4 = text3.Substring(74, 7);
					if (text4.Trim().Length <= 0)
					{
						continue;
					}
					bool flag = false;
					for (int num = 6; num >= 0; num--)
					{
						if (array[num] == text4)
						{
							if (text.Length > 0 && !array4[num].Contains(text))
							{
								ref string reference = ref array4[num];
								reference = reference + ", " + text;
							}
							array5[num]++;
							flag = true;
							break;
						}
					}
					if (!flag)
					{
						if (array[7].Length > 0)
						{
							arrayList.Add(array2[7] + "\r\n" + array[7].Replace(" ", "|") + "\r\n" + array3[7] + "\r\n" + array5[7] + "\r\n" + array4[7]);
						}
						for (int num2 = 7; num2 > 0; num2--)
						{
							array[num2] = array[num2 - 1];
							array2[num2] = array2[num2 - 1];
							array3[num2] = array3[num2 - 1];
							array5[num2] = array5[num2 - 1];
							array4[num2] = array4[num2 - 1];
						}
						array[0] = text4;
						array2[0] = text3.Substring(0, 4) + " " + text3.Substring(4, 2) + " " + text3.Substring(6, 2);
						array3[0] = text3.Substring(18, 7) + text3.Substring(70, 1).Replace("2", "#");
						array5[0] = 1;
						array4[0] = text.PadRight(1);
					}
				}
				while (!streamReader.EndOfStream);
			}
			using StreamWriter streamWriter = new StreamWriter(text2 + "\\Local Graze Index.dat", append: false);
			for (int num3 = 7; num3 >= 0; num3--)
			{
				if (array[num3].Length > 0)
				{
					arrayList.Add(array2[num3] + "\r\n" + array[num3].Replace(" ", "|") + "\r\n" + array3[num3] + "\r\n" + array5[num3] + "\r\n" + array4[num3]);
				}
			}
			arrayList.Sort();
			for (int i = 0; i < arrayList.Count; i++)
			{
				streamWriter.WriteLine(arrayList[i]!.ToString()!.Replace("|", " "));
			}
		}

		public static void ReduceArchiveFile()
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_0038: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to reduce");
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Resource Files\\");
			((FileDialog)val).set_Filter("Archive Files (Archive*.*)|Archive Obs*.*| All files (*.*)|*.*");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Moon", "Moon*.bin");
				for (int i = 0; i < files.Length; i++)
				{
					File.Delete(files[i]);
				}
				ReduceArchiveOccultationFile(((FileDialog)val).get_FileName(), UsingSiteID: false, ExcludeStartStop: true, ExcludeInvalid: true);
			}
		}

		public static void ReduceArchiveOccultationFile(string SourceFile, bool UsingSiteID, bool ExcludeStartStop, bool ExcludeInvalid)
		{
			string text = "";
			int num = 0;
			bool flag = false;
			bool applyLimbCorrn = true;
			string text2 = "";
			ArrayList arrayList = new ArrayList();
			text = ((!UsingSiteID) ? (Path.GetDirectoryName(SourceFile) + "\\R" + Path.GetFileName(SourceFile)) : (Path.GetDirectoryName(SourceFile) + "\\RbySite_" + Path.GetFileName(SourceFile)));
			using (StreamReader streamReader = new StreamReader(SourceFile))
			{
				int num2 = (int)streamReader.BaseStream.Length;
				using StreamWriter streamWriter = new StreamWriter(text);
				if (!UsingSiteID)
				{
					streamWriter.WriteLine("File name      : " + SourceFile);
					streamWriter.WriteLine("Reduction date : " + DateTime.Now.Date.ToString("D"));
					streamWriter.WriteLine("Ephemeris      : " + Utilities.EphemerisBasis());
					if (Utilities.LOLAFileExists)
					{
						streamWriter.WriteLine("Limb basis     : LRO Lunar Orbiter Laser Altimeter [LOLA]");
						applyLimbCorrn = true;
						streamWriter.WriteLine("O-C basis      : limb correction applied");
					}
					else
					{
						streamWriter.WriteLine("Limb basis     : None");
						applyLimbCorrn = false;
						streamWriter.WriteLine("O-C basis      : limb correction NOT applied");
					}
					streamWriter.WriteLine("");
					streamWriter.WriteLine("  ref Observer             Star No.    y  m  d  h  m   s    PLGTCDV     O-C    PA       l     b     AA    limb  scale    P     D      rR    rM     pM  Source");
					streamWriter.WriteLine("");
				}
				Progress = new PBar();
				((Form)Progress).set_StartPosition((FormStartPosition)1);
				((Form)Progress).set_TopMost(true);
				((Control)Progress).Show();
				((Control)Progress).set_Text("Reducing " + SourceFile);
				Progress.pBarFTP.set_Minimum(0);
				Progress.pBarFTP.set_Maximum(1000);
				Progress.pBarFTP.set_Value(0);
				do
				{
					string text3 = streamReader.ReadLine();
					if ((ExcludeStartStop && "MSEO ".IndexOf(text3.Substring(26, 1)) >= 0) || (ExcludeInvalid && ("DIK".IndexOf(text3.Substring(72, 1)) >= 0 || "SUWXYZ".IndexOf(text3.Substring(73, 1)) >= 0)))
					{
						continue;
					}
					if (!int.TryParse(text3.Substring(0, 4), out var result))
					{
						result = 2000;
					}
					if (!int.TryParse(text3.Substring(4, 2), out var result2))
					{
						result2 = 1;
					}
					if (result2 < 1)
					{
						result2 = 1;
					}
					if (!double.TryParse(text3.Substring(6, 2), out var result3))
					{
						result3 = 1.0;
					}
					if (result3 < 1.0)
					{
						result3 = 1.0;
					}
					if (!double.TryParse(text3.Substring(8, 2), out var result4))
					{
						result4 = 0.0;
					}
					if (!double.TryParse(text3.Substring(10, 2), out var result5))
					{
						result5 = 0.0;
					}
					if (!double.TryParse(text3.Substring(12, 6), out var result6))
					{
						result6 = 0.0;
					}
					double num3 = Utilities.JD_from_Date(result, result2, result3);
					num3 += (result4 + result5 / 60.0 + result6 / 3600.0) / 24.0;
					double PE_Correction = 0.0;
					flag = false;
					PE_SetIfNotSpecified_PE(text3, num3, ref PE_Correction, ref flag);
					num3 -= PE_Correction / 3600.0 / 24.0;
					string text4 = text3.Substring(18, 1);
					if (!"RSXAPU".Contains(text4))
					{
						text4 = "X";
					}
					if (!int.TryParse(text3.Substring(19, 6), out var result7))
					{
						result7 = 0;
					}
					string wDScode = text3.Substring(25, 1);
					string text5 = ((!UsingSiteID) ? text3.Substring(188, 20) : text3.Substring(111, 16).Trim().PadRight(10));
					if (text5.Trim().Length < 2)
					{
						text5 = text3.Substring(127, 20);
					}
					if (!int.TryParse(text3.Substring(81, 4).Replace("-", "").Replace("+", ""), out var result8))
					{
						result8 = 0;
					}
					if (!int.TryParse(text3.Substring(85, 2), out var result9))
					{
						result9 = 0;
					}
					if (!double.TryParse(text3.Substring(87, 5), out var result10))
					{
						result10 = 0.0;
					}
					double num4 = ((double)result8 + (double)result9 / 60.0 + result10 / 3600.0) / (180.0 / Math.PI);
					if (text3.Substring(81, 4).Contains("-"))
					{
						num4 = 0.0 - num4;
					}
					if (!int.TryParse(text3.Substring(92, 3).Replace("-", "").Replace("+", ""), out var result11))
					{
						result11 = 0;
					}
					if (!int.TryParse(text3.Substring(95, 2), out var result12))
					{
						result12 = 0;
					}
					if (!double.TryParse(text3.Substring(97, 5), out var result13))
					{
						result13 = 0.0;
					}
					double num5 = ((double)result11 + (double)result12 / 60.0 + result13 / 3600.0) / (180.0 / Math.PI);
					if (text3.Substring(92, 3).Contains("-"))
					{
						num5 = 0.0 - num5;
					}
					if (!int.TryParse(text3.Substring(102, 2), out var result14))
					{
						result14 = 0;
					}
					if (result14 > 0 && result14 < 6)
					{
						result14 = Utilities.RGOdatum_to_ILOCdatum(result14, num4 * (180.0 / Math.PI), num5 * (180.0 / Math.PI));
					}
					if (!double.TryParse(text3.Substring(104, 6), out var result15))
					{
						result15 = 0.0;
					}
					bool height_is_MSL = text3.Substring(110, 1) == "M";
					ReduceAnObservation(num3, num4, num5, result15, height_is_MSL, result14, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, flag, wDScode, applyLimbCorrn, VizierReduction: false, out var Reduction, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
					num++;
					if (UsingSiteID)
					{
						if (result7 > 0 && result > 1850)
						{
							arrayList.Add(text5 + " " + text3.Substring(18, 8) + " " + text3.Substring(0, 4) + " " + text3.Substring(4, 2) + " " + text3.Substring(6, 2) + " " + text3.Substring(8, 2) + " " + text3.Substring(10, 2) + " " + text3.Substring(12, 6) + " " + text3.Substring(26, 3) + text2 + text3.Substring(73, 1) + " " + Reduction.Substring(0, 17));
						}
					}
					else
					{
						text2 = text3.Substring(34, 1) + text3.Substring(42, 1) + text3.Substring(46, 1);
						if (text2.Trim().Length == 0)
						{
							text2 = RGOcodes_to_ILOCcodes(text3.Substring(59, 4));
						}
						streamWriter.WriteLine(num.ToString().PadLeft(6, '0').Substring(1, 5) + " " + text5 + " " + text3.Substring(18, 8) + " " + text3.Substring(0, 4) + " " + text3.Substring(4, 2) + " " + text3.Substring(6, 2) + " " + text3.Substring(8, 2) + " " + text3.Substring(10, 2) + " " + text3.Substring(12, 6) + " " + text3.Substring(26, 3) + text2 + text3.Substring(73, 1) + " " + Reduction);
					}
					if (num % 20 == 0)
					{
						int num6 = (int)streamReader.BaseStream.Position;
						Progress.pBarFTP.set_Value((int)((double)num6 / (0.001 * (double)num2)));
						Application.DoEvents();
					}
				}
				while (!streamReader.EndOfStream);
				((Component)(object)Progress).Dispose();
			}
			if (!UsingSiteID)
			{
				return;
			}
			arrayList.Sort();
			using StreamWriter streamWriter2 = new StreamWriter(text);
			for (int i = 0; i < arrayList.Count; i++)
			{
				streamWriter2.WriteLine(arrayList[i]!.ToString());
			}
		}

		private static void PE_SetIfNotSpecified_PE(string InLine, double JD_atEvent, ref double PE_Correction, ref bool PE_ReductionIfBrightStar)
		{
			if (!"XU".Contains(InLine.Substring(33, 1)))
			{
				return;
			}
			if (InLine.Substring(33, 1) == "U" && double.TryParse(InLine.Substring(29, 4), out var result))
			{
				PE_Correction = result;
				return;
			}
			if ((InLine.Substring(26, 1) == "D") | (InLine.Substring(26, 1) == "B") | (InLine.Substring(26, 1) == "F"))
			{
				PE_Correction = 0.48;
				if (InLine.Substring(34, 1) == "E")
				{
					PE_Correction = 0.07;
				}
			}
			else
			{
				Utilities.QuickMoon(JD_atEvent, out var RA, out var Dec, out var _, out var _, out var MoonLatitude);
				Utilities.QuickPlanet(JD_atEvent, 0, EquinoxOfDate: true, out var RA2, out var Dec2, out MoonLatitude);
				Utilities.Distance(RA2, Dec2, RA, Dec, out var Distance, out var PA_atOrigin);
				Distance *= 180.0 / Math.PI;
				PA_atOrigin *= 180.0 / Math.PI;
				if (PA_atOrigin > 180.0 || PA_atOrigin < 0.0)
				{
					Distance = 360.0 - Distance;
				}
				if (Distance < 180.0)
				{
					PE_Correction = 0.99;
					if (InLine.Substring(34, 1) == "E")
					{
						PE_Correction = 0.5;
					}
				}
				else
				{
					double num = (Distance - 180.0) / 100.0;
					double num2 = -0.79 * num + 0.29 * num * num;
					if (InLine.Substring(34, 1) == "E")
					{
						PE_Correction = 0.5 + num2;
					}
					else
					{
						PE_Correction = 0.99 + num2;
					}
				}
			}
			PE_ReductionIfBrightStar = true;
		}

		public static void CreateVizierArchiveFile()
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Invalid comparison between Unknown and I4
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Invalid comparison between Unknown and I4
			int num = 0;
			bool flag = false;
			bool flag2 = false;
			if ((int)MessageBox.Show("You are about to create the 'Vizier Archive \r\nusing the files in the Resource Files subdirectory.\r\n\r\nIt will take several hours to create the file.\r\n\r\nDo you want to continue?", "Create Vizier Archive", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if ((int)MessageBox.Show("Do you wish to keep the current version of the file\r\n\r\nIf No, the existing file will be overwritten\r\nIf Yes, the extension for the current file will become .bup", "Overwite or backup?", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				string text = Utilities.AppPath + "\\Resource Files\\Vizier Archive.txt";
				string text2 = Utilities.AppPath + "\\Resource Files\\Vizier Archive.bup";
				string text3 = Utilities.AppPath + "\\Resource Files\\Vizier Archive.bp2";
				string text4 = Utilities.AppPath + "\\Resource Files\\Vizier Archive.bp3";
				if (File.Exists(text))
				{
					if (File.Exists(text2))
					{
						if (File.Exists(text3))
						{
							if (File.Exists(text4))
							{
								File.Delete(text4);
							}
							File.Move(text3, text4);
						}
						File.Move(text2, text3);
					}
					File.Move(text, text2);
				}
			}
			string text5 = "Vizier Archive";
			string path = Utilities.AppPath + "\\Resource Files\\" + text5 + ".txt";
			ArrayList arrayList = new ArrayList();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\", "Archive Observations*.dat");
			foreach (string value in files)
			{
				arrayList.Add(value);
			}
			arrayList.Sort();
			files = Directory.GetFiles(AppPath + "\\Resource Files\\Moon", "Moon*.bin");
			for (int i = 0; i < files.Length; i++)
			{
				File.Delete(files[i]);
			}
			using StreamWriter streamWriter = new StreamWriter(path);
			for (int j = 0; j < arrayList.Count; j++)
			{
				using (StreamReader streamReader = new StreamReader(arrayList[j]!.ToString()))
				{
					int num2 = (int)streamReader.BaseStream.Length;
					Progress = new PBar();
					((Form)Progress).set_StartPosition((FormStartPosition)1);
					((Form)Progress).set_TopMost(true);
					((Control)Progress).Show();
					((Control)Progress).set_Text("Reducing " + Path.GetFileName(arrayList[j]!.ToString()));
					Progress.pBarFTP.set_Minimum(0);
					Progress.pBarFTP.set_Maximum(1000);
					Progress.pBarFTP.set_Value(0);
					do
					{
						string text6 = streamReader.ReadLine();
						if (!int.TryParse(text6.Substring(0, 4), out var result))
						{
							result = 2000;
						}
						if (!int.TryParse(text6.Substring(4, 2), out var result2))
						{
							result2 = 1;
						}
						if (result2 < 1)
						{
							result2 = 1;
						}
						if (!double.TryParse(text6.Substring(6, 2), out var result3))
						{
							result3 = 1.0;
						}
						if (result3 < 1.0)
						{
							result3 = 1.0;
						}
						if (!double.TryParse(text6.Substring(8, 2), out var result4))
						{
							result4 = 0.0;
						}
						if (!double.TryParse(text6.Substring(10, 2), out var result5))
						{
							result5 = 0.0;
						}
						if (!double.TryParse(text6.Substring(12, 6), out var result6))
						{
							result6 = 0.0;
						}
						double num3 = Utilities.JD_from_Date(result, result2, result3);
						num3 += (result4 + result5 / 60.0 + result6 / 3600.0) / 24.0;
						double PE_Correction = 0.0;
						flag2 = false;
						PE_SetIfNotSpecified_PE(text6, num3, ref PE_Correction, ref flag2);
						num3 -= PE_Correction / 3600.0 / 24.0;
						string text7 = text6.Substring(18, 1);
						if (!"RSXAPU".Contains(text7))
						{
							text7 = "X";
						}
						if (!int.TryParse(text6.Substring(19, 6), out var result7))
						{
							result7 = 0;
						}
						string wDScode = text6.Substring(25, 1);
						if (text6.Substring(188, 20).Trim().Length < 2)
						{
							text6.Substring(127, 20);
						}
						if (!int.TryParse(text6.Substring(81, 4).Replace("-", "").Replace("+", ""), out var result8))
						{
							result8 = 0;
						}
						if (!int.TryParse(text6.Substring(85, 2), out var result9))
						{
							result9 = 0;
						}
						if (!double.TryParse(text6.Substring(87, 5), out var result10))
						{
							result10 = 0.0;
						}
						double num4 = ((double)result8 + (double)result9 / 60.0 + result10 / 3600.0) / (180.0 / Math.PI);
						if (text6.Substring(81, 4).Contains("-"))
						{
							num4 = 0.0 - num4;
						}
						if (!int.TryParse(text6.Substring(92, 3).Replace("-", "").Replace("+", ""), out var result11))
						{
							result11 = 0;
						}
						if (!int.TryParse(text6.Substring(95, 2), out var result12))
						{
							result12 = 0;
						}
						if (!double.TryParse(text6.Substring(97, 5), out var result13))
						{
							result13 = 0.0;
						}
						double num5 = ((double)result11 + (double)result12 / 60.0 + result13 / 3600.0) / (180.0 / Math.PI);
						if (text6.Substring(92, 3).Contains("-"))
						{
							num5 = 0.0 - num5;
						}
						if (!int.TryParse(text6.Substring(102, 2), out var result14))
						{
							result14 = 0;
						}
						if (result14 > 0 && result14 < 6)
						{
							result14 = Utilities.RGOdatum_to_ILOCdatum(result14, num4 * (180.0 / Math.PI), num5 * (180.0 / Math.PI));
						}
						if (!double.TryParse(text6.Substring(104, 6), out var result15))
						{
							result15 = 0.0;
						}
						bool height_is_MSL = text6.Substring(110, 1) == "M";
						ReduceAnObservation(num3, num4, num5, result15, height_is_MSL, result14, text7, result7, UseOuterLimbOfSatelliteOrAsteroid: false, flag2, wDScode, ApplyLimbCorrn: true, VizierReduction: true, out var Reduction, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
						streamWriter.Write(text6 + " " + Reduction + "\n");
						num++;
						if (num % 20 == 0)
						{
							int num6 = (int)streamReader.BaseStream.Position;
							Progress.pBarFTP.set_Value((int)((double)num6 / (0.001 * (double)num2)));
							Application.DoEvents();
						}
					}
					while (!streamReader.EndOfStream);
					((Component)(object)Progress).Dispose();
					if (flag)
					{
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
		}

		internal static string RGOcodes_to_ILOCcodes(string RGOcodes60_63)
		{
			if (RGOcodes60_63.Trim().Length == 0)
			{
				return "   ";
			}
			if (!int.TryParse(RGOcodes60_63.Substring(3, 1), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(RGOcodes60_63.Substring(0, 1), out var result2))
			{
				result2 = 0;
			}
			if (!int.TryParse(RGOcodes60_63.Substring(2, 1), out var result3))
			{
				result3 = 0;
			}
			string text = " PEXSSCEXT".Substring(result, 1);
			string text2 = " 111232333".Substring(result2, 1);
			string text3 = "        F ".Substring(result3, 1);
			return text + text2 + text3;
		}

		public static void AutoCorrectArchiveFile(string SourceFile, string SourceID)
		{
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ce: Invalid comparison between Unknown and I4
			string text = "";
			double num = 1.0;
			double num2 = 1.0;
			double MoonAlt = 0.0;
			double MoonAlt2 = 0.0;
			int num3 = 0;
			int num4 = 0;
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			int num9 = -1;
			bool flag = false;
			if ((SourceID.Length != 1) | ("AHIMNOPRSX".IndexOf(SourceID) < 0))
			{
				MessageBox.Show("Code for source of observations is set to " + SourceID + "\r\nThis is not a valid code. Valid codes are one of A H I M N O P R S X \r\n\r\nCorrection routine will not proceed", "Erroneous source code", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string text2 = Path.GetDirectoryName(SourceFile) + "\\C_" + Path.GetFileName(SourceFile);
			if (File.Exists(text2) && (int)MessageBox.Show("The destination file  \r\n    " + text2 + "\r\nExists. It will be overwritten, with all manual edits lost\r\n\r\nDo you want to contune?", "Confirm overwrite", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(SourceFile);
			int num10 = (int)streamReader.BaseStream.Length;
			using StreamWriter streamWriter = new StreamWriter(text2);
			Progress = new PBar();
			((Form)Progress).set_StartPosition((FormStartPosition)1);
			((Form)Progress).set_TopMost(true);
			((Control)Progress).Show();
			((Control)Progress).set_Text("Correcting " + SourceFile);
			Progress.pBarFTP.set_Minimum(0);
			Progress.pBarFTP.set_Maximum(100);
			Progress.pBarFTP.set_Value(0);
			do
			{
				string text3 = streamReader.ReadLine();
				if (text3.Substring(12, 6) == "      ")
				{
					text3 = text3.Remove(13, 1).Insert(13, "0");
				}
				if (text3.Substring(18, 1) == " ")
				{
					text3 = text3.Remove(18, 1).Insert(18, "U");
				}
				if (text3.Substring(18, 1) == "U" && text3.Substring(19, 6) != "     0")
				{
					text3 = text3.Remove(19, 6).Insert(19, "     0");
				}
				if (text3.Substring(180, 4) == "   0")
				{
					text3 = text3.Remove(180, 4).Insert(180, "    ");
				}
				if (text3.Substring(184, 4) == "   0")
				{
					text3 = text3.Remove(184, 4).Insert(184, "    ");
				}
				if ("ABCDEFGSWXZ".Contains(text3.Substring(73, 1)))
				{
					bool flag2 = true;
					text = text3;
				}
				else
				{
					bool flag3 = text3.Substring(73, 1) == "D";
					if (!int.TryParse(text3.Substring(0, 4), out var result))
					{
						result = 2000;
					}
					if (!int.TryParse(text3.Substring(4, 2), out var result2))
					{
						result2 = 1;
					}
					if (result2 < 1)
					{
						result2 = 1;
					}
					if (!double.TryParse(text3.Substring(6, 2), out var result3))
					{
						result3 = 1.0;
					}
					if (result3 < 1.0)
					{
						result3 = 1.0;
					}
					if (!double.TryParse(text3.Substring(8, 2), out var result4))
					{
						result4 = 0.0;
					}
					if (!double.TryParse(text3.Substring(10, 2), out var result5))
					{
						result5 = 0.0;
					}
					if (!double.TryParse(text3.Substring(12, 6), out var result6))
					{
						result6 = 0.0;
					}
					string text4 = text3.Substring(18, 1);
					if (!"RSXAPU".Contains(text4))
					{
						text4 = "X";
					}
					if (!int.TryParse(text3.Substring(19, 6), out var result7))
					{
						result7 = 0;
					}
					string wDScode = text3.Substring(25, 1);
					double result8 = 0.0;
					flag = false;
					if (text3.Substring(33, 1) == "N")
					{
						result8 = 0.99;
						if ((text3.Substring(26, 1) == "D") | (text3.Substring(26, 1) == "B") | (text3.Substring(26, 1) == "F"))
						{
							result8 = 0.48;
						}
						flag = true;
					}
					else if (text3.Substring(42, 1) == "U" && !double.TryParse(text3.Substring(29, 4), out result8))
					{
						result8 = 0.0;
					}
					double num11 = Utilities.JD_from_Date(result, result2, result3);
					double num12 = num11 + (result4 + result5 / 60.0 + (result6 - result8) / 3600.0) / 24.0;
					text3.Substring(188, 25);
					if (!int.TryParse(text3.Substring(81, 4).Replace("-", "").Replace("+", ""), out var result9))
					{
						result9 = 0;
					}
					if (!int.TryParse(text3.Substring(85, 2), out var result10))
					{
						result10 = 0;
					}
					if (!double.TryParse(text3.Substring(87, 5), out var result11))
					{
						result11 = 0.0;
					}
					double num13 = ((double)result9 + (double)result10 / 60.0 + result11 / 3600.0) / (180.0 / Math.PI);
					if (text3.Substring(81, 4).Contains("-"))
					{
						num13 = 0.0 - num13;
					}
					if (!int.TryParse(text3.Substring(92, 3).Replace("-", "").Replace("+", ""), out var result12))
					{
						result12 = 0;
					}
					if (!int.TryParse(text3.Substring(95, 2), out var result13))
					{
						result13 = 0;
					}
					if (!double.TryParse(text3.Substring(97, 5), out var result14))
					{
						result14 = 0.0;
					}
					double num14 = ((double)result12 + (double)result13 / 60.0 + result14 / 3600.0) / (180.0 / Math.PI);
					if (text3.Substring(92, 3).Contains("-"))
					{
						num14 = 0.0 - num14;
					}
					if (!int.TryParse(text3.Substring(102, 2), out var result15))
					{
						result15 = 0;
					}
					if (!double.TryParse(text3.Substring(104, 6), out var result16))
					{
						result16 = 0.0;
					}
					ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, flag, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out var Reduction, out var Residual, out MoonAlt2, out var SunAlt, out var Illumination, out var MagStar, out var PA, out var MoonRadius);
					num3++;
					bool flag2 = false;
					Residual = Math.Abs(Residual);
					double num15 = 4.0;
					if (result < 1670)
					{
						num15 = 100.0;
					}
					else if (result < 1700)
					{
						num15 = 50.0;
					}
					else if (result < 1720)
					{
						num15 = 15.0;
					}
					else if (result < 1770)
					{
						num15 = 10.0;
					}
					else if (result < 1850)
					{
						num15 = 7.0;
					}
					double num16 = num15 - 2.0;
					double num17 = num15 + 1.0;
					if (((((Residual < num15) | (text3.Substring(73, 1) == "S") | (text3.Substring(28, 1) == "G")) || result < 1850) && MoonAlt2 > 0.0) & (text4 != "U"))
					{
						flag2 = true;
						text = text3;
						if (result < 1770)
						{
							text = text3.Substring(0, 73) + "T" + text3.Substring(74);
						}
						if (text3.Substring(73, 1) == "Y")
						{
							text = text3.Substring(0, 73) + " " + text3.Substring(74);
						}
					}
					else
					{
						Utilities.QuickMoon(num12, out var RA, out var Dec, out var _, out var _, out var _);
						Utilities.QuickPlanet(num12, 3, EquinoxOfDate: true, out var RA2, out var Dec2, out var _);
						Utilities.Distance(RA2, Dec2, RA, Dec, out var Distance, out var _);
						double num18 = 10.0;
						if (Distance > 2.2)
						{
							num18 = 9.0;
						}
						if (Distance > 2.4)
						{
							num18 = 8.0;
						}
						if (Distance > 2.8)
						{
							num18 = 7.0;
						}
						if (text3.Substring(27, 1) == "B")
						{
							num18 = 4.0;
						}
						if (text3.Substring(27, 1) == "U" || Distance > 3.087)
						{
							num18 = 10.0;
						}
						if (result < 1800)
						{
							num18 = 6.0;
						}
						else if (result < 1850)
						{
							num18 = 7.0;
						}
						num8 = (num7 = (num6 = (num5 = (num4 = 0))));
						if (!flag2 && MoonAlt2 > 0.0)
						{
							IdentifyXZStar(num12, num13, num14, result16, result15, text3.Substring(26, 1), num16, fromAutoCorrect: true, fromManualDetect: false);
							PossibleStars.SortFlag = 1;
							StarList.Sort();
							for (int i = 0; i < StarList.Count; i++)
							{
								if (StarList[i].Mag < num18)
								{
									flag2 = true;
									string text5 = "B";
									if (flag3)
									{
										text5 = "F";
									}
									text = text3.Substring(0, 18) + StarList[i].Number.Substring(0, 1) + StarList[i].Number.Substring(1).PadLeft(6) + text3.Substring(25, 48) + text5 + text3.Substring(74);
									break;
								}
							}
						}
						double Residual2;
						if (!flag2 && result7 > 0)
						{
							for (int j = 1; j <= 20; j++)
							{
								for (num9 = -1; num9 <= 1; num9 += 2)
								{
									ReduceAnObservation(num12 + (double)(j * num9) / 1440.0, num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (((Math.Abs(Residual2) < 1.5 * num16) & (Math.Abs(Residual2) < 0.25 * Residual)) && MoonAlt > 0.0 && SunAlt < 0.0)
									{
										num4 = j * num9;
										flag2 = true;
										break;
									}
								}
								if (flag2)
								{
									break;
								}
							}
						}
						if ((!flag2 && result7 > 0) & (Math.Abs(Residual) > 200.0))
						{
							for (int k = 1; k <= 12; k++)
							{
								for (num9 = -1; num9 <= 1; num9 += 2)
								{
									ReduceAnObservation(num12 + (double)(k * num9) / 24.0, num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num16 && MoonAlt > 0.0 && SunAlt < 0.0)
									{
										num5 = k * num9;
										flag2 = true;
										break;
									}
								}
								if (flag2)
								{
									break;
								}
							}
						}
						if ((!flag2 && result7 > 0) & (Math.Abs(Residual) > 2000.0))
						{
							for (int l = 1; l <= 6; l++)
							{
								for (num9 = -1; num9 <= 1; num9 += 2)
								{
									ReduceAnObservation(num12 + (double)(l * num9), num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num16 && MoonAlt > 0.0 && SunAlt < 0.0)
									{
										num6 = l * num9;
										flag2 = true;
										break;
									}
								}
								if (flag2)
								{
									break;
								}
							}
						}
						if ((!flag2 && result7 > 0) & (Math.Abs(Residual) > 2000.0))
						{
							for (int m = 1; m <= 12; m++)
							{
								if (m != result2)
								{
									num = Utilities.JD_from_Date(result, m, result3) - num11;
									ReduceAnObservation(num12 + num, num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num16 && MoonAlt > 0.0 && SunAlt < 0.0)
									{
										num7 = m;
										flag2 = true;
										break;
									}
								}
							}
						}
						if ((!flag2 && result7 > 0) & (Math.Abs(Residual) > 2000.0))
						{
							for (int n = result - 10; n <= result + 10; n++)
							{
								if (n != result)
								{
									num2 = Utilities.JD_from_Date(n, result2, result3) - num11;
									ReduceAnObservation(num12 + num2, num13, num14, result16, Height_is_MSL: true, result15, text4, result7, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, wDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num16 && MoonAlt > 0.0 && SunAlt < 0.0)
									{
										num8 = n;
										flag2 = true;
										break;
									}
								}
							}
						}
						if (num8 != 0 || num7 != 0 || num6 != 0 || num5 != 0 || num4 != 0)
						{
							if (num8 != 0)
							{
								result = num8;
							}
							else if (num7 != 0)
							{
								result2 = num7;
							}
							else
							{
								result3 += (double)num6;
								result4 += (double)num5;
								result5 += (double)num4;
								if (result5 > 59.0)
								{
									result4 += 1.0;
									result5 -= 60.0;
								}
								if (result5 < 0.0)
								{
									result4 -= 1.0;
									result5 += 60.0;
								}
								if (result4 > 23.0)
								{
									result3 += 1.0;
									result4 -= 24.0;
								}
								if (result4 < 0.0)
								{
									result3 -= 1.0;
									result4 += 24.0;
								}
							}
							Utilities.Date_from_JD(Utilities.JD_from_Date(result, result2, result3), out result, out result2, out result3);
							StringBuilder stringBuilder = new StringBuilder();
							stringBuilder.AppendFormat("{0,4:F0}", result);
							stringBuilder.AppendFormat("{0,2:F0}", result2);
							stringBuilder.AppendFormat("{0,2:F0}", result3);
							stringBuilder.AppendFormat("{0,2:F0}", result4);
							stringBuilder.AppendFormat("{0,2:F0}", result5);
							string text5 = "A";
							if (flag3)
							{
								text5 = "E";
							}
							else
							{
								text = stringBuilder.ToString() + text3.Substring(12, 61) + text5 + text3.Substring(74);
							}
						}
						if (!flag2 && MoonAlt2 > 0.0)
						{
							string text5 = "B";
							if (flag3)
							{
								text5 = "F";
							}
							for (int num19 = 1; num19 <= 7; num19++)
							{
								if (num19 == 3)
								{
									continue;
								}
								ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", num19 * 1000, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
								if (Math.Abs(Residual2) < num17)
								{
									flag2 = true;
									text = text3.Substring(0, 18) + "P" + (num19 * 1000).ToString().PadLeft(6) + text3.Substring(25, 48) + text5 + text3.Substring(74);
									break;
								}
								if ((num19 == 5) & (Math.Abs(Residual2) < 660.0))
								{
									ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", 5001, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num17)
									{
										flag2 = true;
										text = text3.Substring(0, 18) + "P  5001" + text3.Substring(25, 48) + text5 + text3.Substring(74);
										break;
									}
									ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", 5002, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num17)
									{
										flag2 = true;
										text = text3.Substring(0, 18) + "P  5002" + text3.Substring(25, 48) + text5 + text3.Substring(74);
										break;
									}
									ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", 5003, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num17)
									{
										flag2 = true;
										text = text3.Substring(0, 18) + "P  5003" + text3.Substring(25, 48) + text5 + text3.Substring(74);
										break;
									}
									ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", 5004, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num17)
									{
										flag2 = true;
										text = text3.Substring(0, 18) + "P  5004" + text3.Substring(25, 48) + text5 + text3.Substring(74);
										break;
									}
								}
								if ((num19 == 6) & (Math.Abs(Residual2) < 300.0))
								{
									ReduceAnObservation(num12, num13, num14, result16, Height_is_MSL: true, result15, "P", 6006, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, " ", ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual2, out MoonAlt, out SunAlt, out Illumination, out MagStar, out PA, out MoonRadius);
									if (Math.Abs(Residual2) < num17)
									{
										flag2 = true;
										text = text3.Substring(0, 18) + "P  6006" + text3.Substring(25, 48) + text5 + text3.Substring(74);
										break;
									}
								}
							}
						}
						if (!flag2 && MoonAlt2 > 0.0)
						{
							num18 = 12.0;
							if (Distance > 2.2)
							{
								num18 = 11.0;
							}
							if (Distance > 2.4)
							{
								num18 = 10.0;
							}
							if (Distance > 2.8)
							{
								num18 = 9.0;
							}
							if (text3.Substring(27, 1) == "B")
							{
								num18 = 5.0;
							}
							if (text3.Substring(27, 1) == "U" || Distance > 3.087)
							{
								num18 = 13.0;
							}
							if (result < 1800)
							{
								num18 = 6.0;
							}
							else if (result < 1850)
							{
								num18 = 7.0;
							}
							IdentifyXZStar(num12, num13, num14, result16, result15, text3.Substring(26, 1), 1.5 * num16, fromAutoCorrect: true, fromManualDetect: false);
							PossibleStars.SortFlag = 1;
							StarList.Sort();
							for (int num20 = 0; num20 < StarList.Count; num20++)
							{
								if (StarList[num20].Mag < num18)
								{
									flag2 = true;
									string text5 = "B";
									if (flag3)
									{
										text5 = "F";
									}
									text = text3.Substring(0, 18) + StarList[num20].Number.Substring(0, 1) + StarList[num20].Number.Substring(1).PadLeft(6) + text3.Substring(25, 48) + text5 + text3.Substring(74);
									break;
								}
							}
						}
						if (!flag2)
						{
							text = ((result7 != 0) ? (text3.Substring(0, 73) + "Y" + text3.Substring(74)) : (text3.Substring(0, 73) + "U" + text3.Substring(74)));
						}
					}
					if (text.Substring(71, 1) == " ")
					{
						text = text.Substring(0, 71) + SourceID + text.Substring(72);
					}
				}
				streamWriter.WriteLine(text);
				if (num3 % 100 == 0)
				{
					int num21 = (int)streamReader.BaseStream.Position;
					Progress.pBarFTP.set_Value((int)((double)num21 / (0.01 * (double)num10)));
					Application.DoEvents();
				}
			}
			while (!streamReader.EndOfStream);
			((Component)(object)Progress).Dispose();
		}

		public static void CreateHistoricalFileInEditor(string SiteID, bool RGODisplayed, string ObservationLine, string SiteLine, string ObserverLine, string SiteName, bool Merge, bool UseOldFormat)
		{
			string text = "";
			string text2 = "";
			string text3 = "";
			int num = 0;
			PBar pBar = new PBar();
			((Control)pBar).Show();
			((Form)pBar).set_TopMost(true);
			ArrayList arrayList = new ArrayList();
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
			foreach (string path in files)
			{
				arrayList.Add(Path.GetFileName(path));
			}
			EventLine.UseOldFormat = UseOldFormat;
			TelescopeLine.UseOldFormat = UseOldFormat;
			Occult.Lunar_Observations.ObserverLine.UseOldFormat = UseOldFormat;
			Show_ObservationsEditor();
			text = SiteID;
			text2 = ObservationLine.Substring(81, 21);
			if (SiteLine.Trim().Length == 0)
			{
				if (SiteID == "")
				{
					ObserverLine = "OA  " + ObservationLine.Substring(188, 25);
					SiteLine = "TA  " + ObservationLine.Substring(177, 3) + " " + ObservationLine.Substring(180, 4) + " " + ObservationLine.Substring(184, 4) + "   " + ObservationLine.Substring(81, 11) + " " + ObservationLine.Substring(92, 10) + " " + ObservationLine.Substring(102, 2) + " " + ObservationLine.Substring(104, 4) + "  " + ObservationLine.Substring(110, 1);
					SiteName = ObservationLine.Substring(127, 50);
				}
				else if (RGODisplayed)
				{
					RGOsites.OpenRGOSites();
					RGOsites.RGOSite(SiteID.Insert(3, " "), out SiteLine, out SiteName, out var Observer);
					if (ObserverLine.Trim().Length < 4)
					{
						ObserverLine = Observer;
					}
					if (ObserverLine.Trim().Length < 4)
					{
						ObserverLine = SiteName;
					}
					SiteName = SiteName.Substring(4);
					RGOsites.CloseRGOSites();
				}
				else
				{
					ILOCsites.OpenILOCSites();
					ILOCsites.OpenILOCObserver();
					string Observer;
					if (!ILOCsites.ILOCSite(SiteID.Substring(0, 5) + "  " + SiteID.Substring(5, 2), out SiteLine, out SiteName) | (SiteLine.Substring(2, 1) == "*"))
					{
						ILOCsites.ILOCSite(SiteID, out SiteLine, out SiteName);
						Observer = ILOCsites.ILOCObserver(SiteID);
					}
					else
					{
						Observer = ILOCsites.ILOCObserver(SiteID);
					}
					if (ObserverLine.Trim().Length < 4)
					{
						ObserverLine = Observer;
					}
					ILOCsites.CloseILOCSites();
					ILOCsites.CloseILOCObserver();
				}
			}
			if (Merge)
			{
				OccMerge.Events.Clear();
				OccMerge.Observers.Clear();
				OccMerge.Telescopes.Clear();
				OccMerge.DecodeLine(SiteLine.PadRight(80), "", AtEndOfData: true);
				OccMerge.DecodeLine(ObserverLine.PadRight(80), "", AtEndOfData: true);
			}
			else
			{
				OccMain.Events.Clear();
				OccMain.Observers.Clear();
				OccMain.Telescopes.Clear();
				OccMain.Place = Utilities.ProperCase(SiteName).Trim();
				OccMain.EMail = "";
				OccMain.Address = "";
				OccMain.Representative = "Observations retrieved from the Archive files";
				OccMain.Reported = "";
				OccMain.DecodeLine(SiteLine.PadRight(80), "", AtEndOfData: true);
				OccMain.DecodeLine(ObserverLine.PadRight(80), "", AtEndOfData: true);
			}
			pBar.pBarFTP.set_Value(0);
			pBar.pBarFTP.set_Minimum(0);
			pBar.pBarFTP.set_Maximum(100);
			((Control)pBar).set_Visible(true);
			for (int j = 0; j < arrayList.Count; j++)
			{
				pBar.pBarFTP.set_Value(100 * j / arrayList.Count);
				using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\" + arrayList[j]!.ToString());
				do
				{
					text3 = streamReader.ReadLine();
					if (SiteID == "")
					{
						if (text3.Substring(81, 21) == text2)
						{
							num++;
							if (Merge)
							{
								OccMerge.DecodeLine(text3.Substring(0, 59) + "AA", "  ", AtEndOfData: true);
							}
							else
							{
								OccMain.DecodeLine(text3.Substring(0, 59) + "AA", "  ", AtEndOfData: true);
							}
						}
					}
					else if (RGODisplayed)
					{
						if (text == text3.Substring(120, 5))
						{
							num++;
							if (Merge)
							{
								OccMerge.DecodeLine(RGOtoILOC(text3.Substring(0, 63)) + "AA", "  ", AtEndOfData: true);
							}
							else
							{
								OccMain.DecodeLine(RGOtoILOC(text3.Substring(0, 63)) + "AA", "  ", AtEndOfData: true);
							}
						}
					}
					else if (text3.Substring(111, 9) == text)
					{
						num++;
						if (Merge)
						{
							OccMerge.DecodeLine(text3.Substring(0, 59) + "AA", "  ", AtEndOfData: true);
						}
						else
						{
							OccMain.DecodeLine(text3.Substring(0, 59) + "AA", "  ", AtEndOfData: true);
						}
					}
				}
				while (!streamReader.EndOfStream);
			}
			((Form)pBar).Close();
			if (Merge)
			{
				OccMain.ReNumberSites("A");
				OccMain.ReNumberNames("A");
				OccMerge.ReNumberSites(OccMain.GetNextSiteID());
				OccMerge.ReNumberNames(OccMain.GetNextNameID());
				for (int k = 0; k < OccMerge.Telescopes.Count; k++)
				{
					OccMain.AddNewTelescopeLine(OccMerge.Telescopes[k].ToString());
				}
				for (int l = 0; l < OccMerge.Observers.Count; l++)
				{
					OccMain.AddNewObserverLine(OccMerge.Observers[l].ToString());
				}
				for (int m = 0; m < OccMerge.Events.Count; m++)
				{
					OccMerge.Events[m].GetEventLines(out var Line, out var Line2);
					if (Line2.Trim().Length < 1)
					{
						Line2 = "";
					}
					OccMain.AddNewEventLine(Line, Line2);
				}
				EventLine.SortField = 3;
				OccMain.Events.Sort();
			}
			OccMain.ReNumberEvents();
			Observations_Editor.DisplayReport(AllForms: true, HighlightLast: false);
		}

		private static string RGOtoILOC(string InLine)
		{
			string[] array = new string[10] { "     ", "0.2  ", "0.4  ", "0.6  ", "0.9  ", "1.   ", "1.   ", "1.   ", "1.   ", "9.   " };
			string text = InLine.Substring(0, 59);
			if (InLine.Substring(59, 4).Trim().Length > 0)
			{
				if (!int.TryParse(InLine.Substring(59, 1), out var result))
				{
					result = 0;
				}
				if (!int.TryParse(InLine.Substring(60, 1), out var result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(InLine.Substring(61, 1), out var result3))
				{
					result3 = 0;
				}
				if (!int.TryParse(InLine.Substring(62, 1), out var result4))
				{
					result4 = 0;
				}
				if (result2 == 8)
				{
					text = text.Remove(27, 1).Insert(27, "U");
				}
				if (result4 > 0)
				{
					string value = " PEXSSCEXT".Substring(result4, 1) + " " + "  RCRTMCCR".Substring(result4, 1);
					text = text.Remove(34, 3).Insert(34, value);
				}
				if (result > 0)
				{
					string text2 = array[result];
					string text3 = " 111232223".Substring(result, 1);
					text = text.Remove(37, 6).Insert(37, text2 + text3);
				}
				if (result3 == 8)
				{
					text = text.Remove(46, 1).Insert(46, "B");
				}
				if (result2 > 0)
				{
					string text4 = " 112211111".Substring(result2, 1) + " 123311111".Substring(result2, 1) + "  567123   ".Substring(result2, 1);
					if (result == 6 && (result2 < 2 || result2 > 7))
					{
						text4 = text4.Substring(0, 2) + "4";
					}
					text = text.Remove(53, 3).Insert(53, text4);
				}
			}
			return text;
		}

		internal static void Create_ILOC_Observers()
		{
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			ArrayList arrayList = new ArrayList();
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
			for (int i = 0; i < files.Length; i++)
			{
				using StreamReader streamReader = new StreamReader(files[i]);
				do
				{
					string text = streamReader.ReadLine();
					string text2 = text.Substring(111, 9);
					if (text2.Trim().Length > 0 && text2.Substring(0, 2) != "XX" && text.Substring(188, 25).Trim().Length > 0)
					{
						arrayList.Add(text2 + " " + text.Substring(188, 25));
					}
				}
				while (!streamReader.EndOfStream);
			}
			arrayList.Sort();
			using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\Archive ILOC Observers.dat"))
			{
				for (int j = 1; j < arrayList.Count; j++)
				{
					if (arrayList[j]!.ToString()!.Substring(0, 9) != arrayList[j - 1]!.ToString()!.Substring(0, 9))
					{
						streamWriter.WriteLine(arrayList[j]!.ToString());
					}
				}
			}
			MessageBox.Show("Creation of ILOC Names file complete", "Observer names", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		internal static void Create_ILOC_Sites()
		{
			//IL_0148: Unknown result type (might be due to invalid IL or missing references)
			ArrayList arrayList = new ArrayList();
			string[] files = Directory.GetFiles(AppPath + "\\Resource Files\\", "Archive Observations*.*");
			for (int i = 0; i < files.Length; i++)
			{
				using StreamReader streamReader = new StreamReader(files[i]);
				do
				{
					string text = streamReader.ReadLine();
					string text2 = text.Substring(111, 9);
					if (text2.Trim().Length > 0 && text2.Substring(0, 2) != "XX")
					{
						arrayList.Add(text2 + " " + text.Substring(81, 30) + text.Substring(127, 61));
					}
				}
				while (!streamReader.EndOfStream);
			}
			arrayList.Sort();
			using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\Archive ILOC Sites.dat"))
			{
				for (int j = 1; j < arrayList.Count; j++)
				{
					if (arrayList[j]!.ToString()!.Substring(0, 9) != arrayList[j - 1]!.ToString()!.Substring(0, 9))
					{
						streamWriter.WriteLine(arrayList[j]!.ToString());
					}
				}
			}
			MessageBox.Show("Creation of ILOC Sites file complete", "Site names", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		internal static void VerifyReportData(bool ShowIfNoErrors)
		{
			string text = "";
			for (int i = 0; i < OccMain.Telescopes.Count; i++)
			{
				string text2 = "";
				if (!" RNCO".Contains(OccMain.Telescopes[i].TelescopeType))
				{
					text2 = text2 + ", TelType(" + OccMain.Telescopes[i].TelescopeType + ")";
				}
				if (!" EA".Contains(OccMain.Telescopes[i].MountType))
				{
					text2 = text2 + ", Mount(" + OccMain.Telescopes[i].MountType + ")";
				}
				if (!" DM".Contains(OccMain.Telescopes[i].DriveType))
				{
					text2 = text2 + ", Drive(" + OccMain.Telescopes[i].DriveType + ")";
				}
				if ((OccMain.Telescopes[i].Aperture < 0.0) | (OccMain.Telescopes[i].Aperture > 190.0))
				{
					text2 = text2 + ", Aperture(" + OccMain.Telescopes[i].Aperture + ")";
				}
				if ((OccMain.Telescopes[i].FocalLength < 0.0) | (OccMain.Telescopes[i].FocalLength > 800.0))
				{
					text2 = text2 + ", FL(" + OccMain.Telescopes[i].FocalLength + ")";
				}
				if ((OccMain.Telescopes[i].LongDeg < 0) | (OccMain.Telescopes[i].LongDeg > 180))
				{
					text2 = text2 + ", LongDeg(" + OccMain.Telescopes[i].LongDeg + ")";
				}
				if ((OccMain.Telescopes[i].LongMin < 0) | (OccMain.Telescopes[i].LongMin > 59))
				{
					text2 = text2 + ", LongMin(" + OccMain.Telescopes[i].LongMin + ")";
				}
				if ((OccMain.Telescopes[i].LongSec < 0.0) | (OccMain.Telescopes[i].LongSec >= 60.0))
				{
					text2 = text2 + ", LongSec(" + OccMain.Telescopes[i].LongSec + ")";
				}
				if ((OccMain.Telescopes[i].LatDeg < 0) | (OccMain.Telescopes[i].LatDeg > 180))
				{
					text2 = text2 + ", LatDeg(" + OccMain.Telescopes[i].LatDeg + ")";
				}
				if ((OccMain.Telescopes[i].LatMin < 0) | (OccMain.Telescopes[i].LatMin > 59))
				{
					text2 = text2 + ", LatMin(" + OccMain.Telescopes[i].LatMin + ")";
				}
				if ((OccMain.Telescopes[i].LatSec < 0.0) | (OccMain.Telescopes[i].LatSec >= 60.0))
				{
					text2 = text2 + ", LatSec(" + OccMain.Telescopes[i].LatSec + ")";
				}
				if (OccMain.Telescopes[i].DatumNumber < 1)
				{
					text2 = text2 + ", Horizontal Datum(" + OccMain.Telescopes[i].DatumNumber + ")";
				}
				if ((OccMain.Telescopes[i].Altitude < -200.0) | (OccMain.Telescopes[i].Altitude > 5000.0) | (OccMain.Telescopes[i].Altitude == 0.0))
				{
					text2 = text2 + ", Altitude(" + OccMain.Telescopes[i].Altitude + ")";
				}
				if (!"EM".Contains(OccMain.Telescopes[i].VerticalDatum))
				{
					text2 = text2 + ", VerDatum(" + OccMain.Telescopes[i].VerticalDatum + ")";
				}
				if (text2.Length > 0)
				{
					text = text + "Tel " + OccMain.Telescopes[i].TelescopeCodeForEvent + text2 + "\r\n";
				}
			}
			for (int j = 0; j < OccMain.Events.Count; j++)
			{
				string text2 = "";
				if ((OccMain.Events[j].Year < 1623) | (OccMain.Events[j].Year > 2100))
				{
					text2 = text2 + ", Year(" + OccMain.Events[j].Year + ")";
				}
				if ((OccMain.Events[j].Month < 1) | (OccMain.Events[j].Month > 12))
				{
					text2 = text2 + ", Month(" + OccMain.Events[j].Month + ")";
				}
				if ((OccMain.Events[j].Day < 1) | (OccMain.Events[j].Day > 31))
				{
					text2 = text2 + ", Day(" + OccMain.Events[j].Day + ")";
				}
				if ((OccMain.Events[j].Hour < 0) | (OccMain.Events[j].Hour > 23))
				{
					text2 = text2 + ", Hr(" + OccMain.Events[j].Hour + ")";
				}
				if ((OccMain.Events[j].Minute < 0) | (OccMain.Events[j].Minute > 59))
				{
					text2 = text2 + ", Min(" + OccMain.Events[j].Minute + ")";
				}
				if ((OccMain.Events[j].Second < 0.0) | (OccMain.Events[j].Second >= 60.0))
				{
					text2 = text2 + ", Sec(" + OccMain.Events[j].Second + ")";
				}
				if (!"RSXAPU".Contains(OccMain.Events[j].StarCat))
				{
					text2 = text2 + ", StarCat(" + OccMain.Events[j].StarCat + ")";
				}
				else if (OccMain.Events[j].StarCat == "R")
				{
					if ((OccMain.Events[j].StarNumber < 0) | (OccMain.Events[j].StarNumber > 3537))
					{
						text2 = text2 + ", ZC(" + OccMain.Events[j].StarNumber + ")";
					}
				}
				else if (OccMain.Events[j].StarCat == "S")
				{
					if ((OccMain.Events[j].StarNumber < 57982) | (OccMain.Events[j].StarNumber >= 210563))
					{
						text2 = text2 + ", SAO(" + OccMain.Events[j].StarNumber + ")";
					}
				}
				else if (OccMain.Events[j].StarCat == "X" && ((OccMain.Events[j].StarNumber < 0) | (OccMain.Events[j].StarNumber > 244437)))
				{
					text2 = text2 + ", XZ(" + OccMain.Events[j].StarNumber + ")";
				}
				if (!" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".Contains(OccMain.Events[j].WDS))
				{
					text2 = text2 + ", WDS(" + OccMain.Events[j].WDS + ")";
				}
				if (!"DRBFMSEO".Contains(OccMain.Events[j].OccEvent))
				{
					text2 = text2 + ", Phase(" + OccMain.Events[j].OccEvent + ")";
				}
				if (!"DBU".Contains(OccMain.Events[j].Limb))
				{
					text2 = text2 + ", Limb(" + OccMain.Events[j].Limb + ")";
				}
				if (!" G".Contains(OccMain.Events[j].GrazeFlag))
				{
					text2 = text2 + ", GrazeFlag(" + OccMain.Events[j].GrazeFlag + ")";
				}
				if ((OccMain.Events[j].PE < 0.0) | (OccMain.Events[j].PE > 2.0))
				{
					text2 = text2 + ", PE(" + OccMain.Events[j].PE + ")";
				}
				if (!"SABUEX".Contains(OccMain.Events[j].PEApplication))
				{
					text2 = text2 + ", PEAppl(" + OccMain.Events[j].PEApplication + ")";
				}
				if (!" GVMSTEPKXC".Contains(OccMain.Events[j].MethodRecording1))
				{
					text2 = text2 + ", Method1(" + OccMain.Events[j].MethodRecording1 + ")";
				}
				if (!" GVMSTEPKXCA".Contains(OccMain.Events[j].MethodRecording2))
				{
					text2 = text2 + ", Method2(" + OccMain.Events[j].MethodRecording2 + ")";
				}
				if (!" GRNCTMO".Contains(OccMain.Events[j].MethodTiming))
				{
					text2 = text2 + ", Time(" + OccMain.Events[j].MethodTiming + ")";
				}
				if ((OccMain.Events[j].Accuracy < 0.0) | ((OccMain.Events[j].Accuracy > 2.0) & (OccMain.Events[j].Year > 1750)))
				{
					text2 = text2 + ", Acc(" + OccMain.Events[j].Accuracy + ")";
				}
				if ((OccMain.Events[j].Certainty < 1) | (OccMain.Events[j].Certainty > 3))
				{
					text2 = text2 + ", Cert(" + OccMain.Events[j].Certainty + ")";
				}
				if ((OccMain.Events[j].SignalToNoise < 0.0) | (OccMain.Events[j].SignalToNoise > 9.9))
				{
					text2 = text2 + ", S/N(" + OccMain.Events[j].SignalToNoise + ")";
				}
				if (!" WENSBFU".Contains(OccMain.Events[j].DoubleStarComponent))
				{
					text2 = text2 + ", Double(" + OccMain.Events[j].DoubleStarComponent + ")";
				}
				if ((OccMain.Events[j].Duration < 0.0) | (OccMain.Events[j].Duration > 2.0))
				{
					text2 = text2 + ", Durn(" + OccMain.Events[j].Duration + ")";
				}
				if (!" TF".Contains(OccMain.Events[j].LightLevel))
				{
					text2 = text2 + ", LightLevel(" + OccMain.Events[j].LightLevel + ")";
				}
				if ((OccMain.Events[j].Stability < 0) | (OccMain.Events[j].Stability > 3))
				{
					text2 = text2 + ", Stab(" + OccMain.Events[j].Stability + ")";
				}
				if ((OccMain.Events[j].Transparency < 0) | (OccMain.Events[j].Transparency > 3))
				{
					text2 = text2 + ", Trans(" + OccMain.Events[j].Transparency + ")";
				}
				if ((OccMain.Events[j].Circumstances < 0) | (OccMain.Events[j].Circumstances > 9))
				{
					text2 = text2 + ", Remark(" + OccMain.Events[j].Circumstances + ")";
				}
				if ((OccMain.Events[j].Temperature < -30) | (OccMain.Events[j].Temperature > 50))
				{
					text2 = text2 + ", Temp(" + OccMain.Events[j].Temperature + ")";
				}
				if ((OccMain.Events[j].MethodRecording1.ToString() == " ") & (OccMain.Events[j].MethodRecording2.ToString() != " "))
				{
					text2 += ", First (left) MethodBox empty";
				}
				if (text2.Length > 0)
				{
					text = text + "Line " + OccMain.Events[j].SeqNumber + text2 + "\r\n";
				}
			}
			if (!(text.Length == 0 && !ShowIfNoErrors))
			{
				try
				{
					((Control)Errors).Show();
				}
				catch
				{
					Errors = new ErrorReport();
					((Control)Errors).Show();
				}
				if (text.Length == 0)
				{
					((Control)Errors.txtErrors).set_Text("No errors detected in the current observations");
				}
				else
				{
					((Control)Errors.txtErrors).set_Text("The following errors (or possible errors) have been detected in the current observations\r\n\r\n" + text);
				}
				((Form)Errors).set_TopMost(true);
			}
		}

		internal static void ListPossibledoubles()
		{
			string text = "";
			for (int i = 0; i < OccMain.Events.Count; i++)
			{
				if ("WENSBF".Contains(OccMain.Events[i].DoubleStarComponent))
				{
					text = text + "Line " + OccMain.Events[i].SeqNumber.ToString().PadRight(3) + "  Code = " + OccMain.Events[i].DoubleStarComponent + "   WDS code = " + OccMain.Events[i].WDS + "\r\n";
				}
				if ((OccMain.Events[i].Circumstances == 1) & !OccMain.Events[i].Duration_Valid)
				{
					text = text + "Line " + OccMain.Events[i].SeqNumber.ToString().PadRight(3) + "  Gradual duration not set\r\n";
				}
			}
			try
			{
				((Control)DoubleList).Show();
			}
			catch
			{
				DoubleList = new DoubleList();
				((Control)DoubleList).Show();
			}
			if (text.Length == 0)
			{
				((Control)DoubleList.txtDoubles).set_Text("No doubles detected in the current observations");
			}
			else
			{
				((Control)DoubleList.txtDoubles).set_Text(text);
			}
			((Form)DoubleList).set_TopMost(true);
		}

		internal static void VerifyAccuracyValues()
		{
			//IL_00b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b8: Expected I4, but got Unknown
			Observations_Editor.optEvents.set_Checked(true);
			for (int i = 0; i < OccMain.Events.Count; i++)
			{
				if (OccMain.Events[i].Accuracy > 0.99)
				{
					((ListControl)Observations_Editor.lstEvents).set_SelectedIndex(i);
					string text = Observations_Editor.lstEvents.get_Items().get_Item(i).ToString();
					switch ((int)MessageBox.Show("Event: " + text.Substring(0, 44) + "\r\n\r\n Accuracy value is set at '" + text.Substring(41, 3) + "' seconds.\r\n\r\nDo you want to divide by 10?", "Divide Accuracy by 10?", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152))
					{
					case 6:
						OccMain.Events[i].Accuracy = OccMain.Events[i].Accuracy / 10.0;
						OccMain.Events[i].Accuracy_DecPlaces = 1;
						Observations_Editor.DisplayEventsInEditor(i);
						break;
					case 2:
						return;
					}
				}
			}
		}

		internal static void VerifyTemperatureValues()
		{
			//IL_00af: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected I4, but got Unknown
			Observations_Editor.optEvents.set_Checked(true);
			for (int i = 0; i < OccMain.Events.Count; i++)
			{
				if (OccMain.Events[i].Temperature > 60)
				{
					((ListControl)Observations_Editor.lstEvents).set_SelectedIndex(i);
					string text = Observations_Editor.lstEvents.get_Items().get_Item(i).ToString()!.PadRight(65);
					switch ((int)MessageBox.Show("Event: " + text.Trim() + "\r\n\r\n Temperatue value is set at " + text.Substring(60, 3) + " degrees Celsius.\r\n\r\nDo you want to subtract 100?", "Temperature - subtract 100?", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152))
					{
					case 6:
						OccMain.Events[i].Temperature = OccMain.Events[i].Temperature - 100;
						Observations_Editor.DisplayEventsInEditor(i);
						break;
					case 2:
						return;
					}
				}
			}
		}
	}
}

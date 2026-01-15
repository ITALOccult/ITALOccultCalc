using System;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class GetStarPosition
	{
		internal static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		internal static bool HaveCheckedForCatalogues;

		private static bool TychoGaiaPresent;

		private static bool NOMADpresent;

		private static bool UCAC4present;

		private static bool UCAC3present;

		private static bool UCAC2present;

		private static bool Tycho2present;

		private static bool PPMXLpresent;

		private static bool URAT1present;

		private static bool XZpresent;

		internal static CompareStarCats CompareCatalogues;

		public static void ShowCompareCatalogues(bool fromAsteroidPrediction)
		{
			try
			{
				((Control)CompareCatalogues).Show();
			}
			catch
			{
				CompareCatalogues = new CompareStarCats(fromAsteroidPrediction);
				((Control)CompareCatalogues).Show();
			}
			((Control)CompareCatalogues).Focus();
		}

		internal static void CheckStarCatsPresent()
		{
			XZpresent = File.Exists(AppPath + "\\Resource Files\\xz80.dat");
			TychoGaiaPresent = File.Exists(AppPath + "\\Resource Files\\Gaia\\TychoGaia.bin");
			Tycho2present = File.Exists(AppPath + "\\Resource Files\\Tycho2.bin");
			UCAC4present = File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z001");
			UCAC3present = File.Exists(Settings.Default.UCAC3_Path + "\\z360");
			UCAC2present = File.Exists(Settings.Default.UCAC2_Path + "\\u2index.da") & File.Exists(Settings.Default.UCAC2_Path + "\\z200");
			PPMXLpresent = File.Exists(Settings.Default.PPMXL_Path + "\\n89d.dat");
			string text = Settings.Default.NOMAD_Path;
			if (!text.ToLower().Contains("nomad") | !File.Exists(text + "\\090\\m0900.cat"))
			{
				text = Settings.Default.NOMAD_Short_path;
			}
			NOMADpresent = File.Exists(text + "\\090\\m0900.cat") & File.Exists(text + "\\090\\m0900.inx");
			URAT1present = File.Exists(Settings.Default.URAT1_Path + "\\z900");
			HaveCheckedForCatalogues = true;
		}

		internal static bool GetHipparcosPosition(int Hip, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagR, out double MagV, out double MagB, out double Parallax, out double Epoch, out bool UsedGaia, out string CorrectStarIdentifier)
		{
			int GaiaVersion;
			ulong GaiaSourceID;
			string SourceFile;
			double RV;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarDiameter_mas;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			return GetHipparcosPosition(Hip, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out CorrectStarIdentifier);
		}

		internal static bool GetHipparcosPosition(int Hip, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RV, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4, out string CorrectStarIdentifier)
		{
			UsedGaia = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (RV = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0))))))))))))));
			DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1));
			GaiaSourceID = 0uL;
			GaiaVersion = 0;
			Epoch = 2000.0;
			bool hIPfromGaia = Gaia.GetHIPfromGaia(Hip, out SourceFile);
			if (hIPfromGaia)
			{
				RA = Gaia.RA_rad;
				pmRA = Gaia.PMRA_rad;
				Dec = Gaia.Dec_rad;
				pmDec = Gaia.PMDec_rad;
				MagB = Gaia.MagBlue;
				MagV = Gaia.MagGreen;
				MagR = Gaia.MagRed;
				Parallax_asec = Gaia.Parallax_asec;
				Epoch = 2000.0 + Gaia.Epoch_2000;
				UsedGaia = true;
				GaiaVersion = Gaia.GaiaVersionOfStar;
				GaiaSourceID = Gaia.Source_ID;
				RV = Gaia.RadialVelocityKmSec;
				UncertRA = Gaia.SDev_RA_mas;
				UncertDec = Gaia.SDev_Dec_mas;
				UncertPMRA = Gaia.SDev_pmRA_mas_yr;
				UncertPMDec = Gaia.SDev_pmDec_mas_yr;
				StarDiameter_mas = Gaia.StarDiameter_mas;
				StarReliability = Gaia.Reliability;
				DuplicateSource = Gaia.DuplicateSource;
				NoGaiaPM = Gaia.NoGaiaProperMotion;
				GaiaPMfromUCAC4 = Gaia.ProperMotionUsingUCAC4;
				CorrectStarIdentifier = "";
			}
			CorrectStarIdentifier = Gaia.StarID;
			return hIPfromGaia;
		}

		internal static bool GetGaiaPosition(string ID, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out string SourceFile, out string StarNumber, out string TruncatedString, out double StarDiameter_mas)
		{
			int GaiaVersion;
			ulong GaiaSourceID;
			double RV;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			return GetGaiaPosition(ID, 16.0, 2.0, 10.0, FilterUsingStarMag: false, LimitUsingStarMag: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC, out StarNumber, out TruncatedString);
		}

		internal static bool GetGaiaPosition(string ID_asCoordinateString, double ObservationDate_yrsFrom2000, double MatchDistanceArcSec, double StarMag_ForFilter, bool FilterUsingStarMag, bool LimitUsingStarMag, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RV, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4, out string StarNumber, out string TruncatedString)
		{
			double RA_ID_deg = 0.0;
			double Dec_ID_deg = 0.0;
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (Epoch = (RV = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0)))))))))))))));
			DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1));
			GaiaVersion = 0;
			GaiaSourceID = 0uL;
			SourceFile = "";
			UsedGaia = false;
			StarNumber = "";
			if (!StarCoords_to_RA_Dec(ID_asCoordinateString, out RA_ID_deg, out Dec_ID_deg, out TruncatedString))
			{
				return false;
			}
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			bool num = Gaia.Get_GaiaStar_fromGaia(RA_ID_deg, Dec_ID_deg, StarCoords_used_as_ID: true, ObservationDate_yrsFrom2000, MatchDistanceArcSec, StarMag_ForFilter, FilterUsingStarMag, LimitUsingStarMag, out SourceFile);
			if (num)
			{
				RA = Gaia.RA_rad;
				pmRA = Gaia.PMRA_rad;
				Dec = Gaia.Dec_rad;
				pmDec = Gaia.PMDec_rad;
				MagB = Gaia.MagBlue;
				MagV = Gaia.MagGreen;
				MagR = Gaia.MagRed;
				Parallax_asec = Gaia.Parallax_asec;
				Epoch = 2000.0 + Gaia.Epoch_2000;
				UsedGaia = true;
				GaiaVersion = Gaia.GaiaVersionOfStar;
				GaiaSourceID = Gaia.Source_ID;
				SourceFile = Gaia.SourceOfData;
				RV = Gaia.RadialVelocityKmSec;
				UncertRA = Gaia.SDev_RA_mas;
				UncertDec = Gaia.SDev_Dec_mas;
				UncertPMRA = Gaia.SDev_pmRA_mas_yr;
				UncertPMDec = Gaia.SDev_pmDec_mas_yr;
				StarDiameter_mas = Gaia.StarDiameter_mas;
				StarReliability = Gaia.Reliability;
				DuplicateSource = Gaia.DuplicateSource;
				NoGaiaPM = Gaia.NoGaiaProperMotion;
				GaiaPMfromUCAC4 = Gaia.ProperMotionUsingUCAC4;
				StarNumber = Gaia.StarID;
			}
			return num;
		}

		internal static bool StarCoords_to_RA_Dec(string ID, out double RA_ID_deg, out double Dec_ID_deg, out string TruncatedString)
		{
			TruncatedString = ID;
			for (int num = ID.Length - 1; num >= 0; num--)
			{
				string value = ID.Substring(num, 1);
				if (!"+-.0123456789".Contains(value))
				{
					ID = ID.Remove(num, 1);
					TruncatedString = ID;
				}
			}
			RA_ID_deg = (Dec_ID_deg = 0.0);
			int num2 = ID.IndexOf("+");
			if (num2 < 0)
			{
				num2 = ID.IndexOf("-");
			}
			if (num2 < 0)
			{
				return false;
			}
			string text = ID.Substring(0, num2);
			string text2 = ID.Substring(num2 + 1);
			RA_ID_deg = 15.0 * double.Parse(text.Substring(0, 2)) + double.Parse(text.Substring(2, 2)) / 4.0 + double.Parse(text.Substring(4)) / 240.0;
			Dec_ID_deg = double.Parse(text2.Substring(0, 2)) + double.Parse(text2.Substring(2, 2)) / 60.0 + double.Parse(text2.Substring(4)) / 3600.0;
			if (ID.Substring(num2, 1) == "-")
			{
				Dec_ID_deg = 0.0 - Dec_ID_deg;
			}
			return true;
		}

		internal static bool GetTycho2Position(int Region, int SeqNum, int Component, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax, out bool UsedGaia, out double Epoch)
		{
			int GaiaVersion;
			ulong GaiaSourceID;
			string SourceFile;
			double RadialVelocity;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarDiameter_mas;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			return GetTycho2Position(Region, SeqNum, Component, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
		}

		internal static bool GetTycho2Position(int Region, int SeqNum, int Component, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RadialVelocity, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4)
		{
			bool flag = false;
			Epoch = 2000.0;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (Epoch = (RadialVelocity = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0)))))))))))))));
			int NumMeasures = (DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1)));
			GaiaSourceID = 0uL;
			GaiaVersion = 0;
			SourceFile = "None";
			UsedGaia = false;
			flag = Gaia.GetTycho2fromGaiaDR2(Region, SeqNum, Component, out SourceFile);
			if (flag)
			{
				RA = Gaia.RA_rad;
				pmRA = Gaia.PMRA_rad;
				Dec = Gaia.Dec_rad;
				pmDec = Gaia.PMDec_rad;
				MagB = Gaia.MagBlue;
				MagV = Gaia.MagGreen;
				MagR = Gaia.MagRed;
				Parallax_asec = Gaia.Parallax_asec;
				Epoch = 2000.0 + Gaia.Epoch_2000;
				UsedGaia = true;
				GaiaVersion = Gaia.GaiaVersionOfStar;
				GaiaSourceID = Gaia.Source_ID;
				RadialVelocity = Gaia.RadialVelocityKmSec;
				UncertRA = Gaia.SDev_RA_mas;
				UncertDec = Gaia.SDev_Dec_mas;
				UncertPMRA = Gaia.SDev_pmRA_mas_yr;
				UncertPMDec = Gaia.SDev_pmDec_mas_yr;
				StarDiameter_mas = Gaia.StarDiameter_mas;
				StarReliability = Gaia.Reliability;
				DuplicateSource = Gaia.DuplicateSource;
				NoGaiaPM = Gaia.NoGaiaProperMotion;
				GaiaPMfromUCAC4 = Gaia.ProperMotionUsingUCAC4;
			}
			if (!flag && UCAC4present && UCAC4.UCAC4_from_Tycho2(Region, SeqNum, Component, out var U4Zone, out var U4Seq))
			{
				flag = GetUCAC4Position(U4Zone, U4Seq, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC4);
			}
			string Basis;
			bool InvalidDiameter;
			if (!flag && Tycho2present)
			{
				flag = Tycho2.GetTycho2fromTycho2(Region, SeqNum, Component);
				if (flag)
				{
					RA = Tycho2.RA;
					pmRA = Tycho2.PMRA;
					Dec = Tycho2.Dec;
					pmDec = Tycho2.PMDec;
					MagB = Tycho2.MagB;
					MagV = Tycho2.MagV;
					MagR = 25.0;
					Parallax_asec = Tycho2.Parallax_asec;
					Epoch = 2000 + Tycho2.TRA;
					UsedGaia = false;
					GaiaVersion = 1;
					GaiaSourceID = 0uL;
					SourceFile = "TychoGaia";
					RadialVelocity = 0.0;
					UncertRA = Tycho2.SDev_RA;
					UncertDec = Tycho2.SDev_Dec;
					UncertPMRA = Tycho2.SDev_pmRA;
					UncertPMDec = Tycho2.SDev_pmDec;
					StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
					StarReliability = 0.0;
					DuplicateSource = -1;
					NoGaiaPM = -1;
					GaiaPMfromUCAC4 = -1;
				}
			}
			if (!flag && NOMADpresent)
			{
				NOMAD.InitialiseNOMAD();
				flag = NOMAD.Tycho2(Region, SeqNum, Component);
				NOMAD.ReleaseNOMAD();
				if (flag)
				{
					RA = NOMAD.RA;
					pmRA = NOMAD.PM_ra;
					Dec = NOMAD.Dec;
					pmDec = NOMAD.PM_dec;
					MagV = NOMAD.Mv;
					MagB = NOMAD.Mb;
					MagR = NOMAD.Mr;
					Parallax_asec = 0.0;
					Epoch = 2000.0;
					UsedGaia = false;
					GaiaVersion = -1;
					GaiaSourceID = 0uL;
					SourceFile = "NOMAD";
					RadialVelocity = 0.0;
					UncertRA = NOMAD.SDev_RA;
					UncertDec = NOMAD.SDev_Dec;
					UncertPMRA = NOMAD.SDev_pmRA;
					UncertPMDec = NOMAD.SDev_pmDec;
					StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
					StarReliability = 0.0;
					DuplicateSource = -1;
					NoGaiaPM = -1;
					GaiaPMfromUCAC4 = -1;
				}
			}
			return flag;
		}

		internal static bool GetUCAC4Position(int U4Zone, int U4Number, bool Parallax_IfNotInGaia_TryHipparcos, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax, out bool UsedGaia, out double Epoch)
		{
			int GaiaVersion;
			ulong GaiaSourceID;
			string SourceFile;
			double RadialVelocity;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarDiameter_mas;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			return GetUCAC4Position(U4Zone, U4Number, Parallax_IfNotInGaia_TryHipparcos, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
		}

		internal static bool GetUCAC4Position(int U4Zone, int U4Number, bool Parallax_IfNotInGaia_TryHipparcos, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax, out double Epoch, out bool UsedGaia, out string SourceFile, out double RV)
		{
			int GaiaVersion;
			ulong GaiaSourceID;
			double UncertRA;
			double UncertDec;
			double UncertPMRA;
			double UncertPMDec;
			double StarDiameter_mas;
			double StarReliability;
			int DuplicateSource;
			int NoGaiaPM;
			int GaiaPMfromUCAC;
			return GetUCAC4Position(U4Zone, U4Number, Parallax_IfNotInGaia_TryHipparcos, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out Epoch, out UsedGaia, out GaiaVersion, out GaiaSourceID, out SourceFile, out RV, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC);
		}

		internal static bool GetUCAC4Position(int U4Zone, int U4Number, bool Parallax_IfNotInGaia_TryHipparcos, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RadialVelocity, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4)
		{
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (Epoch = (RadialVelocity = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0)))))))))))))));
			int NumMeasures = (DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1)));
			GaiaVersion = 0;
			GaiaSourceID = 0uL;
			SourceFile = "None";
			UsedGaia = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			if (Gaia.Get_U4_fromGaia_DR3_EDR3(U4Zone, U4Number, out SourceFile))
			{
				RA = Gaia.RA_rad;
				pmRA = Gaia.PMRA_rad;
				Dec = Gaia.Dec_rad;
				pmDec = Gaia.PMDec_rad;
				MagB = Gaia.MagBlue;
				MagV = Gaia.MagGreen;
				MagR = Gaia.MagRed;
				Parallax_asec = Gaia.Parallax_asec;
				Epoch = 2000.0 + Gaia.Epoch_2000;
				UsedGaia = true;
				GaiaVersion = Gaia.GaiaVersionOfStar;
				GaiaSourceID = Gaia.Source_ID;
				RadialVelocity = Gaia.RadialVelocityKmSec;
				UncertRA = Gaia.SDev_RA_mas;
				UncertDec = Gaia.SDev_Dec_mas;
				UncertPMRA = Gaia.SDev_pmRA_mas_yr;
				UncertPMDec = Gaia.SDev_pmDec_mas_yr;
				StarDiameter_mas = Gaia.StarDiameter_mas;
				StarReliability = Gaia.Reliability;
				DuplicateSource = Gaia.DuplicateSource;
				NoGaiaPM = Gaia.NoGaiaProperMotion;
				GaiaPMfromUCAC4 = Gaia.ProperMotionUsingUCAC4;
				return true;
			}
			if (!UCAC4present)
			{
				return false;
			}
			bool num = UCAC4.Read_UCAC_Number(U4Zone, U4Number, UseHipparcos: false);
			if (num)
			{
				RA = UCAC4.RA;
				pmRA = UCAC4.PM_ra;
				Dec = UCAC4.Dec;
				pmDec = UCAC4.PM_dec;
				MagV = UCAC4.MagV;
				MagB = UCAC4.MagB;
				MagR = UCAC4.MagR;
				Parallax_asec = 0.0;
				Epoch = 2000.0;
				UsedGaia = false;
				GaiaVersion = -1;
				SourceFile = "UCAC4";
				RadialVelocity = 0.0;
				UncertRA = UCAC4.SDevRA_mas;
				UncertDec = UCAC4.SDev_Dec_mas;
				UncertPMRA = UCAC4.SDev_pmRA_masyr;
				UncertPMDec = UCAC4.SDev_pmDec_masyr;
				StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out var _, out NumMeasures, out var _);
				StarReliability = 0.0;
				DuplicateSource = -1;
				NoGaiaPM = -1;
				GaiaPMfromUCAC4 = -1;
			}
			return num;
		}

		internal static bool GetPPMXLPosition(string Zone, int PPMXLNumber, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR)
		{
			bool result = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			RA = (pmRA = (Dec = (pmDec = (MagV = (MagB = (MagR = 0.0))))));
			if (!PPMXLpresent)
			{
				return result;
			}
			PPMXL.Read_PPMXL_Number(Zone, PPMXLNumber);
			RA = PPMXL.RA_J2000;
			pmRA = PPMXL.PM_RA;
			Dec = PPMXL.Dec_J2000;
			pmDec = PPMXL.PM_Dec;
			MagV = PPMXL.mag_V;
			MagB = PPMXL.mag_B;
			MagR = PPMXL.mag_R;
			return true;
		}

		internal static bool GetB1Position(int Zone, int SeqNum, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RadialVelocity, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4)
		{
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (Epoch = (RadialVelocity = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0)))))))))))))));
			int NumMeasures = (DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1)));
			GaiaSourceID = 0uL;
			GaiaVersion = 0;
			SourceFile = "None";
			UsedGaia = false;
			bool result = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			if (NOMADpresent)
			{
				NOMAD.InitialiseNOMAD();
				result = NOMAD.USNO_B1(Zone, SeqNum, out var _);
				NOMAD.ReleaseNOMAD();
				RA = NOMAD.RA;
				pmRA = NOMAD.PM_ra;
				Dec = NOMAD.Dec;
				pmDec = NOMAD.PM_dec;
				MagV = NOMAD.Mv;
				MagB = NOMAD.Mb;
				MagR = NOMAD.Mr;
				Parallax_asec = 0.0;
				Epoch = 2000.0;
				UsedGaia = false;
				GaiaVersion = -1;
				GaiaSourceID = 0uL;
				SourceFile = "NOMAD";
				RadialVelocity = 0.0;
				UncertRA = NOMAD.SDev_RA;
				UncertDec = NOMAD.SDev_Dec;
				UncertPMRA = NOMAD.SDev_pmRA;
				UncertPMDec = NOMAD.SDev_pmDec;
				StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out var _, out NumMeasures, out var _);
				StarReliability = 0.0;
				DuplicateSource = -1;
				NoGaiaPM = -1;
				GaiaPMfromUCAC4 = -1;
			}
			return result;
		}

		internal static bool GetNOMAD_Full_Position(int Zone, int SeqNum, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Parallax_asec, out double Epoch, out bool UsedGaia, out int GaiaVersion, out ulong GaiaSourceID, out string SourceFile, out double RadialVelocity, out double UncertRA, out double UncertDec, out double UncertPMRA, out double UncertPMDec, out double StarDiameter_mas, out double StarReliability, out int DuplicateSource, out int NoGaiaPM, out int GaiaPMfromUCAC4)
		{
			RA = (pmRA = (Dec = (pmDec = (MagR = (MagV = (MagB = (Parallax_asec = (Epoch = (RadialVelocity = (UncertRA = (UncertDec = (UncertPMRA = (UncertPMDec = (StarDiameter_mas = (StarReliability = 0.0)))))))))))))));
			int NumMeasures = (DuplicateSource = (NoGaiaPM = (GaiaPMfromUCAC4 = -1)));
			GaiaVersion = 0;
			GaiaSourceID = 0uL;
			SourceFile = "None";
			UsedGaia = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			if (NOMADpresent & !Settings.Default.NOMAD_Path.ToUpper().Contains("_SHORT"))
			{
				NOMAD.Open_Nomad_Catalogue_and_Index_Files(Zone);
				NOMAD.Read_NOMAD_entry(SeqNum - 1);
				NOMAD.Close_Nomad_Catalogue_and_Index_Files();
				RA = NOMAD.RA;
				pmRA = NOMAD.PM_ra;
				Dec = NOMAD.Dec;
				pmDec = NOMAD.PM_dec;
				MagV = NOMAD.Mv;
				MagB = NOMAD.Mb;
				MagR = NOMAD.Mr;
				Parallax_asec = 0.0;
				Epoch = 2000.0;
				UsedGaia = false;
				GaiaVersion = -1;
				SourceFile = "NOMAD";
				RadialVelocity = 0.0;
				UncertRA = NOMAD.SDev_RA;
				UncertDec = NOMAD.SDev_Dec;
				UncertPMRA = NOMAD.SDev_pmRA;
				UncertPMDec = NOMAD.SDev_pmDec;
				StarDiameter_mas = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, MagB, MagR, out var _, out NumMeasures, out var _);
				StarReliability = 0.0;
				DuplicateSource = -1;
				NoGaiaPM = -1;
				GaiaPMfromUCAC4 = -1;
			}
			return false;
		}

		internal static bool GetXZPosition(int XZNumber, out double RA, out double Dec, out double pmRA, out double pmDec, out double MagV, out double MagB, out double MagR, out double Epoch)
		{
			bool result = false;
			if (!HaveCheckedForCatalogues)
			{
				CheckStarCatsPresent();
			}
			RA = (pmRA = (Dec = (pmDec = (MagV = (MagB = (MagR = 0.0))))));
			Epoch = 2000.0;
			if (!XZpresent)
			{
				return result;
			}
			XZ80Q.Get_XZ_Star(XZNumber);
			RA = XZ80Q.RA_rad;
			pmRA = XZ80Q.PMRA_rad;
			Dec = XZ80Q.Dec_rad;
			pmDec = XZ80Q.PMDec_rad;
			MagV = XZ80Q.Mv;
			MagB = XZ80Q.Mp;
			MagR = XZ80Q.Mr;
			Epoch = 2000.0;
			return true;
		}
	}
}

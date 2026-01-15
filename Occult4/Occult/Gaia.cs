using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	internal class Gaia
	{
		private const double Radian = 180.0 / Math.PI;

		private const double MilliSecInDeg = 3600000.0;

		private const double MicroSecInDeg = 3600000000.0;

		internal static string[] StarSourceCat = new string[10] { "Hip2", "GaiaDR1", "GaiaDR2", "GaiaEDR3", "GaiaDR3", "", "", "", "", "UBSC" };

		internal static string[] AllGaiaFiles = new string[1] { "" };

		internal static List<string> GaiaPrimaryFiles = new List<string>();

		internal static List<string> GaiaAdditionalFiles = new List<string>();

		internal static string[] ReservedGaiaFiles = new string[11]
		{
			"gaia16_dr3", "gaia14_dr3", "gaia12_dr3", "gaia9_dr3", "gaia16_edr3", "gaia14_edr3", "gaia12_edr3", "gaia9_edr3", "gaia14_dr2", "gaia11_dr2",
			"Tychogaia"
		};

		internal static string[] Gaia16Files = new string[3] { "Gaia16_DR3", "Gaia16_EDR3", "Gaia16_DR2" };

		internal static string[] Gaia14Files = new string[3] { "Gaia14_DR3", "Gaia14_EDR3", "Gaia14_DR2" };

		internal static string[] Gaia12Files = new string[3] { "Gaia12_DR3", "Gaia12_EDR3", "Gaia12_DR2" };

		internal static string[] Gaia9Files = new string[3] { "Gaia9_DR3", "Gaia9_EDR3", "Gaia9_DR2" };

		internal static bool Gaia16Exists = false;

		internal static bool Gaia14Exists = false;

		internal static bool Gaia12Exists = false;

		internal static bool Gaia9Exists = false;

		internal static bool[] GaiaCatIndex_16_14_12_9 = new bool[4];

		internal static int[] GaiaPrimaryFilesIndex_16_14_12_9 = new int[4] { -1, -1, -1, -1 };

		internal static string[] GaiaCatIDs = new string[4] { "UCAC4 ", "TYC ", "HIP ", "J" };

		internal const long DR3_EDR3_RecordLength = 58L;

		internal const long DR2_RecordLength = 48L;

		internal static string GaiaCatalogueForNearbyStars = "";

		internal static bool GaiaCatalogueForNearbyStarsSet = false;

		internal static long CurrentRecordLength = 58L;

		internal static GaiaCatalogueMap GaiaMap;

		internal static int rAmas = 0;

		internal static int dECmas = 0;

		internal static byte rAmuas = 0;

		internal static byte dECmuas = 0;

		internal static ulong source_ID;

		internal static int pmRA_muasec_yrCosDec;

		internal static int pmDec_muasec_yr;

		internal static ushort parallax_masX80 = 0;

		internal static ushort sDev_RA_10muas;

		internal static ushort sDev_Dec_10muas;

		internal static ushort sDev_Par_10muas;

		internal static ushort sDev_pmRA_muas_yr = 0;

		internal static ushort sDev_pmDec_muas_yr = 0;

		internal static short epoch2000 = 0;

		internal static short epochDec_myrs2000 = 0;

		internal static short rV_kms;

		internal static short mBlue = 30;

		internal static short mRed = 30;

		internal static short mGreen;

		internal static uint catNumber = 0u;

		internal static byte sDev_RV;

		internal static byte spec;

		internal static byte gaiaVersionOfStar;

		internal static byte flags;

		internal static byte catID;

		internal static byte supNum;

		internal static byte starDiameter_mas_x5 = 0;

		internal static string catalogueFormat = "DR2";

		internal static byte reliability;

		private static StringBuilder TG;

		internal static List<UBSC> UBSCstars;

		internal UBSC UBSC;

		internal static bool UBSCchecked = false;

		internal static bool UBSCisLoaded = false;

		internal static List<BadHipStars> BadHipStars;

		internal BadHipStars Bad_HipStars;

		internal static bool BadHipStarsLoaded = false;

		internal static bool BadHipStarsLoaded_Checked = false;

		internal static string CorrectedStarID = "";

		public static string CatalogueFormat
		{
			get
			{
				return catalogueFormat;
			}
			set
			{
				catalogueFormat = value;
			}
		}

		public static double RA_rad => RA_deg / (180.0 / Math.PI);

		public static double RA_deg => ((double)rAmas + (double)(int)rAmuas / 250.0) / 3600000.0;

		public static double Dec_rad => Dec_deg / (180.0 / Math.PI);

		public static double Dec_deg => ((double)dECmas + (double)(int)dECmuas / 250.0) / 3600000.0;

		public static double Parallax_rad => Parallax_asec / 3600.0 / (180.0 / Math.PI);

		public static double Parallax_asec
		{
			get
			{
				return (double)(int)parallax_masX80 / 80000.0;
			}
			set
			{
				parallax_masX80 = Convert.ToUInt16(value * 80000.0);
			}
		}

		public static double PMRA_rad => (double)pmRA_muasec_yrCosDec / 3600000000.0 / Math.Cos(Dec_rad) / (180.0 / Math.PI);

		public static double PMRA_deg => (double)pmRA_muasec_yrCosDec / 3600000000.0 / Math.Cos(Dec_rad);

		public static double PMDec_rad => PMDec_deg / (180.0 / Math.PI);

		public static double PMDec_deg => (double)pmDec_muasec_yr / 3600000000.0;

		public static double RadialVelocityKmSec => rV_kms;

		public static double Epoch_2000 => (double)epoch2000 / 1000.0;

		public static double MagBlue => (double)mBlue / 1000.0;

		public static double MagGreen => (double)mGreen / 1000.0;

		public static double MagRed => (double)mRed / 1000.0;

		public static double SDev_RA_mas => (double)(int)sDev_RA_10muas / 100.0;

		public static double SDev_Dec_mas => (double)(int)sDev_Dec_10muas / 100.0;

		public static double SDev_Parallax_mas => (double)(int)sDev_Par_10muas / 1000.0;

		public static double SDev_pmRA_mas_yr => (double)(int)sDev_pmRA_muas_yr / 1000.0;

		public static double SDev_pmDec_mas_yr => (double)(int)sDev_pmDec_muas_yr / 1000.0;

		public static double SDev_RV => (int)sDev_RV;

		public static byte GaiaVersionOfStar => gaiaVersionOfStar;

		public static ulong Source_ID => source_ID;

		public static double Reliability
		{
			get
			{
				if (RecordLength == 58)
				{
					return (double)(int)reliability / 20.0;
				}
				return -1.0;
			}
		}

		public static byte Flags => flags;

		public static int DuplicateSource
		{
			get
			{
				if (RecordLength == 58)
				{
					return flags & 1;
				}
				return -1;
			}
		}

		public static int NoGaiaProperMotion
		{
			get
			{
				if (RecordLength == 58)
				{
					return (flags & 2) / 2;
				}
				return -1;
			}
		}

		public static int ProperMotionUsingUCAC4
		{
			get
			{
				if (RecordLength == 58)
				{
					return (flags & 4) / 4;
				}
				return -1;
			}
		}

		public static bool UCAC4_PoorMatch => (flags & 8) == 8;

		public static bool HipPositionFrome_UBSC => (flags & 0x40) == 64;

		public static bool HipPositionUnreliable_UBSC => (flags & 0x80) == 128;

		public static double StarDiameter_mas => (double)(int)starDiameter_mas_x5 / 10.0;

		public static uint StarNumber => catNumber;

		public static int StarCatID => catID;

		public static string StarID => CurrentStarID();

		public static string SourceOfData
		{
			get
			{
				if (RecordLength == 48)
				{
					if (Epoch_2000 == -8.75)
					{
						return " from Hipparcos";
					}
					if (Epoch_2000 == 15.0)
					{
						return " from Tgas";
					}
					if ((StarCatID == 4) & (supNum == 2))
					{
						return " pm using UCAC4";
					}
					if ((StarCatID == 4) & (supNum == 4))
					{
						return " poor match to UCAC4";
					}
					if (Epoch_2000 == 17.0)
					{
						return " from Hipparcos/UBSC";
					}
					return "";
				}
				if (GaiaVersionOfStar == 0)
				{
					return " from Hipparcos";
				}
				if (GaiaVersionOfStar == 1)
				{
					return " from Tgas";
				}
				if (ProperMotionUsingUCAC4 == 1)
				{
					return " pm using UCAC4";
				}
				if (GaiaVersionOfStar == 9)
				{
					return " from UBSC";
				}
				return "";
			}
		}

		public static long RecordLength => CurrentRecordLength;

		internal static string Gaia_ASCII_line
		{
			get
			{
				TG = new StringBuilder();
				TG.Append(CurrentStarID().PadRight(20));
				TG.AppendFormat("{0,5:F2} ", (double)mGreen / 1000.0);
				if (RecordLength == 58)
				{
					TG.AppendFormat("{0,5:F2} ", (double)mBlue / 1000.0);
				}
				TG.AppendFormat("{0,5:F2}   ", (double)mRed / 1000.0);
				TG.Append(Utilities.DEGtoDMS(RA_deg / 15.0, 2, 6, MinutesOnly: false));
				TG.AppendFormat(" {0,9:F6} ", (double)pmRA_muasec_yrCosDec / 15.0 / 1000000.0 / Math.Cos(Dec_deg / (180.0 / Math.PI)));
				TG.AppendFormat("{0,5:F2}", SDev_RA_mas);
				TG.AppendFormat(" {0,5:F2}", SDev_pmRA_mas_yr);
				TG.AppendFormat(" {0,7:F2}   ", Epoch_2000 + 2000.0);
				TG.Append(Utilities.DEGtoDMS(Dec_deg, 3, 5, MinutesOnly: false));
				TG.AppendFormat(" {0,8:F5} ", (double)pmDec_muasec_yr / 1000000.0);
				TG.AppendFormat("{0,5:F2}", SDev_Dec_mas);
				TG.AppendFormat(" {0,5:F2}", SDev_pmDec_mas_yr);
				TG.AppendFormat(" {0,7:F2}", Epoch_2000 + 2000.0);
				TG.AppendFormat("  {0,4:F0}", rV_kms);
				TG.AppendFormat("  {0,6:F2}", Parallax_asec * 1000.0);
				TG.AppendFormat("  {0,4:F2}", SDev_Parallax_mas);
				if (RecordLength == 58)
				{
					TG.AppendFormat("  {0,5:f1}", StarDiameter_mas);
					TG.AppendFormat("  {0,5:f2}", Reliability);
					if ((Reliability > 2.0) | (Reliability == 0.0))
					{
						TG.Append("!");
					}
					else if (Reliability > 1.4)
					{
						TG.Append("?");
					}
					else
					{
						TG.Append(" ");
					}
					if (DuplicateSource == 1)
					{
						TG.Append(" d");
					}
					else
					{
						TG.Append(" .");
					}
					if (NoGaiaProperMotion == 1)
					{
						TG.Append(" n");
					}
					else
					{
						TG.Append(" .");
					}
					if (ProperMotionUsingUCAC4 == 1)
					{
						TG.Append(" u");
					}
					else
					{
						TG.Append(" .");
					}
					if (UCAC4_PoorMatch)
					{
						TG.Append(" p");
					}
					else
					{
						TG.Append(" .");
					}
					TG.Append(Source_ID.ToString().PadLeft(21));
				}
				TG.Append("   " + SourceOfData);
				return TG.ToString();
			}
		}

		internal static void GetAvailableGaiaCatalogues()
		{
			if (GaiaPrimaryFiles.Count > 0)
			{
				return;
			}
			AllGaiaFiles = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Gaia\\", "Gaia*.bin");
			Gaia16Exists = (Gaia14Exists = (Gaia12Exists = (Gaia9Exists = false)));
			GaiaPrimaryFiles = new List<string>();
			int num = 0;
			bool flag = false;
			GaiaCatIndex_16_14_12_9[0] = false;
			for (int j = 0; j < Gaia16Files.Length; j++)
			{
				for (int k = 0; k < AllGaiaFiles.Length; k++)
				{
					if (Path.GetFileNameWithoutExtension(AllGaiaFiles[k])!.ToUpper() == Gaia16Files[j].ToUpper())
					{
						GaiaPrimaryFiles.Add(Gaia16Files[j].Replace("g", "G").Replace("dr", "DR"));
						flag = true;
						Gaia16Exists = true;
						GaiaCatIndex_16_14_12_9[0] = true;
						GaiaPrimaryFilesIndex_16_14_12_9[0] = num;
						num++;
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			flag = false;
			GaiaCatIndex_16_14_12_9[1] = false;
			for (int l = 0; l < Gaia14Files.Length; l++)
			{
				for (int m = 0; m < AllGaiaFiles.Length; m++)
				{
					if (Path.GetFileNameWithoutExtension(AllGaiaFiles[m])!.ToUpper() == Gaia14Files[l].ToUpper())
					{
						GaiaPrimaryFiles.Add(Gaia14Files[l].Replace("g", "G").Replace("dr", "DR"));
						flag = true;
						Gaia14Exists = true;
						GaiaCatIndex_16_14_12_9[1] = true;
						GaiaPrimaryFilesIndex_16_14_12_9[1] = num;
						num++;
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			flag = false;
			GaiaCatIndex_16_14_12_9[2] = false;
			for (int n = 0; n < Gaia12Files.Length; n++)
			{
				for (int num2 = 0; num2 < AllGaiaFiles.Length; num2++)
				{
					if (Path.GetFileNameWithoutExtension(AllGaiaFiles[num2])!.ToUpper() == Gaia12Files[n].ToUpper())
					{
						GaiaPrimaryFiles.Add(Gaia12Files[n].Replace("g", "G").Replace("dr", "DR"));
						flag = true;
						Gaia12Exists = true;
						GaiaCatIndex_16_14_12_9[2] = true;
						GaiaPrimaryFilesIndex_16_14_12_9[2] = num;
						num++;
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			flag = false;
			GaiaCatIndex_16_14_12_9[3] = false;
			for (int num3 = 0; num3 < Gaia9Files.Length; num3++)
			{
				for (int num4 = 0; num4 < AllGaiaFiles.Length; num4++)
				{
					if (Path.GetFileNameWithoutExtension(AllGaiaFiles[num4])!.ToUpper() == Gaia9Files[num3].ToUpper())
					{
						GaiaPrimaryFiles.Add(Gaia9Files[num3].Replace("g", "G").Replace("dr", "DR"));
						flag = true;
						Gaia9Exists = true;
						GaiaCatIndex_16_14_12_9[3] = true;
						GaiaPrimaryFilesIndex_16_14_12_9[3] = num;
						break;
					}
				}
				if (flag)
				{
					break;
				}
			}
			string[] AllFiles = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Gaia\\", "*.bin");
			GaiaAdditionalFiles = new List<string>();
			int i;
			for (i = 0; i < AllFiles.Length; i++)
			{
				if (!Array.Exists(ReservedGaiaFiles, (string element) => element == Path.GetFileNameWithoutExtension(AllFiles[i])!.ToLower().Replace(".bin", "")))
				{
					GaiaAdditionalFiles.Add(Path.GetFileNameWithoutExtension(AllFiles[i])!.ToLower().Replace(".bin", ""));
				}
			}
			GaiaAdditionalFiles.Sort();
		}

		internal static string GetBestGaiaCatalogForStarMag(double StarMag)
		{
			int num = -1;
			if (StarMag < 8.5)
			{
				num = 3;
			}
			else if (StarMag < 11.5)
			{
				num = 2;
			}
			else if (StarMag < 13.5)
			{
				num = 1;
			}
			else if (StarMag < 16.2)
			{
				num = 0;
			}
			if ((num == 3) & !GaiaCatIndex_16_14_12_9[3])
			{
				num = 2;
			}
			if ((num == 2) & !GaiaCatIndex_16_14_12_9[2])
			{
				num = 1;
			}
			if ((num == 1) & !GaiaCatIndex_16_14_12_9[1])
			{
				num = 0;
			}
			if ((num == 0) & !GaiaCatIndex_16_14_12_9[0])
			{
				num = -1;
			}
			if (num >= 0)
			{
				return GaiaPrimaryFiles[GaiaPrimaryFilesIndex_16_14_12_9[num]];
			}
			return GaiaPrimaryFiles[0];
		}

		public static double SigmaRA_arcsecs(double T2000)
		{
			double num = SDev_RA_mas / 1000.0;
			double num2 = SDev_pmRA_mas_yr / 1000.0 * (T2000 - Epoch_2000);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static double SigmaDec_arcsecs(double T2000)
		{
			double num = SDev_Dec_mas / 1000.0;
			double num2 = SDev_pmDec_mas_yr / 1000.0 * (T2000 - Epoch_2000);
			return Math.Sqrt(num * num + num2 * num2);
		}

		internal static bool ReadNext(FileStream GaiaFile, BinaryReader Gaia, int RecordNumber)
		{
			if (!UBSCchecked)
			{
				CreateUBSClist();
				UBSCchecked = true;
			}
			if (RecordNumber < 0)
			{
				return false;
			}
			if (RecordLength == 48)
			{
				long num = RecordNumber * RecordLength;
				if (num + RecordLength > GaiaFile.Length)
				{
					return false;
				}
				GaiaFile.Seek(num, SeekOrigin.Begin);
				rAmas = Gaia.ReadInt32();
				rAmuas = Gaia.ReadByte();
				dECmas = Gaia.ReadInt32();
				dECmuas = Gaia.ReadByte();
				parallax_masX80 = Gaia.ReadUInt16();
				pmRA_muasec_yrCosDec = Gaia.ReadInt32();
				pmDec_muasec_yr = Gaia.ReadInt32();
				rV_kms = Gaia.ReadInt16();
				epoch2000 = Gaia.ReadInt16();
				epochDec_myrs2000 = Gaia.ReadInt16();
				mRed = Gaia.ReadInt16();
				mGreen = Gaia.ReadInt16();
				sDev_RA_10muas = Gaia.ReadUInt16();
				sDev_Dec_10muas = Gaia.ReadUInt16();
				sDev_Par_10muas = Gaia.ReadUInt16();
				sDev_pmRA_muas_yr = Gaia.ReadUInt16();
				sDev_pmDec_muas_yr = Gaia.ReadUInt16();
				sDev_RV = Gaia.ReadByte();
				gaiaVersionOfStar = 2;
				spec = Gaia.ReadByte();
				catID = Gaia.ReadByte();
				catNumber = Gaia.ReadUInt32();
				supNum = Gaia.ReadByte();
			}
			else
			{
				long num2 = (long)((double)RecordNumber * (double)RecordLength);
				if (num2 + RecordLength > GaiaFile.Length)
				{
					return false;
				}
				GaiaFile.Seek(num2, SeekOrigin.Begin);
				rAmas = Gaia.ReadInt32();
				rAmuas = Gaia.ReadByte();
				dECmas = Gaia.ReadInt32();
				dECmuas = Gaia.ReadByte();
				parallax_masX80 = Gaia.ReadUInt16();
				pmRA_muasec_yrCosDec = Gaia.ReadInt32();
				pmDec_muasec_yr = Gaia.ReadInt32();
				rV_kms = Gaia.ReadInt16();
				epoch2000 = Gaia.ReadInt16();
				mBlue = Gaia.ReadInt16();
				mGreen = Gaia.ReadInt16();
				mRed = Gaia.ReadInt16();
				sDev_RA_10muas = Gaia.ReadUInt16();
				sDev_Dec_10muas = Gaia.ReadUInt16();
				sDev_Par_10muas = Gaia.ReadUInt16();
				sDev_pmRA_muas_yr = Gaia.ReadUInt16();
				sDev_pmDec_muas_yr = Gaia.ReadUInt16();
				sDev_RV = Gaia.ReadByte();
				reliability = Gaia.ReadByte();
				flags = Gaia.ReadByte();
				starDiameter_mas_x5 = Gaia.ReadByte();
				gaiaVersionOfStar = Gaia.ReadByte();
				source_ID = Gaia.ReadUInt64();
				catID = Gaia.ReadByte();
				catNumber = Gaia.ReadUInt32();
				supNum = 0;
				if ((catID > 2) & (catID < 6))
				{
					supNum = (byte)(catID - 2);
				}
			}
			CorrectedStarID = "";
			if (gaiaVersionOfStar == 0 && GetBadHipStar_EntryName(StarID, out CorrectedStarID))
			{
				return false;
			}
			int hipNum = (int)catNumber;
			bool flag = false;
			if (((MagGreen < 5.0) & !StarID.Contains("HIP")) && GetHipNumFromNonHipNumbers(StarID, out var HipNumber))
			{
				hipNum = HipNumber;
				flag = true;
			}
			if (((gaiaVersionOfStar == 0) | ((catID == 2) & ((Reliability > 1.6) | (Reliability == 0.0) | (MagGreen < 4.0))) | (flag & ((Reliability > 1.6) | (MagGreen < 4.0)))) && UBSCisLoaded)
			{
				Get_UBSC_Data(hipNum, ref rAmas, ref rAmuas, ref dECmas, ref dECmuas, ref parallax_masX80, ref pmRA_muasec_yrCosDec, ref pmDec_muasec_yr, ref sDev_RA_10muas, ref sDev_Dec_10muas, ref sDev_pmRA_muas_yr, ref sDev_pmDec_muas_yr, ref epoch2000, ref gaiaVersionOfStar, ref flags);
			}
			return true;
		}

		internal static void Gaia_FrameRotationCorrections(int GaiaVersion, double JD, double RA, double Dec, double Mag, out double dRA_asec, out double dDec_asec, out double dRA_asec_Uncert, out double dDec_asec_Uncert)
		{
			dRA_asec = (dDec_asec = (dRA_asec_Uncert = (dDec_asec_Uncert = 0.0)));
			if (GaiaVersion < 2 || GaiaVersion > 3)
			{
				return;
			}
			double num = 11.0;
			double num2 = 12.5;
			switch (GaiaVersion)
			{
			case 2:
				num = 11.0;
				num2 = 12.5;
				if (!(Mag >= num2))
				{
					double num9 = 0.137;
					double num10 = 0.245;
					double num11 = 0.045;
					double num12 = 0.051;
					double num13 = 0.042;
					double num14 = 0.036;
					double num15 = -0.019;
					double num16 = 1.304;
					double num17 = 0.553;
					double num18 = -0.051;
					double num19 = -0.014;
					double num20 = num15 * Math.Cos(RA) * Math.Sin(Dec) + num16 * Math.Sin(RA) * Math.Sin(Dec) - num17 * Math.Cos(Dec);
					dDec_asec = (0.0 - num15) * Math.Sin(RA) + num16 * Math.Cos(RA);
					double num21 = -0.068 * Math.Cos(RA) * Math.Sin(Dec) + num18 * Math.Sin(RA) * Math.Sin(Dec) - num19 * Math.Cos(Dec);
					double num22 = (0.0 - -0.068) * Math.Sin(RA) + num18 * Math.Cos(RA);
					double num8 = (JD - 2457206.285) / 365.25;
					dRA_asec = (num20 + num8 * num21) / 1000.0;
					dDec_asec = (dDec_asec + num8 * num22) / 1000.0;
					double num23 = Math.Sqrt(Math.Pow(num9 * Math.Cos(RA) * Math.Sin(Dec), 2.0) + Math.Pow(num10 * Math.Sin(RA) * Math.Sin(Dec), 2.0) + Math.Pow(num11 * Math.Cos(Dec), 2.0));
					dDec_asec_Uncert = Math.Sqrt(Math.Pow(num9 * Math.Sin(RA), 2.0) + Math.Pow(num10 * Math.Cos(RA), 2.0));
					double num24 = num8 * Math.Sqrt(Math.Pow(num12 * Math.Cos(RA) * Math.Sin(Dec), 2.0) + Math.Pow(num13 * Math.Sin(RA) * Math.Sin(Dec), 2.0) + Math.Pow(num14 * Math.Cos(Dec), 2.0));
					double num25 = num8 * Math.Sqrt(Math.Pow(num12 * Math.Sin(RA), 2.0) + Math.Pow(num13 * Math.Cos(RA), 2.0));
					dRA_asec_Uncert = Math.Sqrt(num23 * num23 + num24 * num24) / 1000.0;
					dDec_asec_Uncert = Math.Sqrt(dDec_asec_Uncert * dDec_asec_Uncert + num25 * num25) / 1000.0;
					if (Mag > num && Mag < num2)
					{
						double num26 = (num2 - Mag) / (num2 - num);
						dRA_asec *= num26;
						dDec_asec *= num26;
						dRA_asec_Uncert *= num26;
						dDec_asec_Uncert *= num26;
					}
				}
				break;
			case 3:
				num = 0.0;
				num2 = 13.0;
				if (!(Mag >= num2 || Mag < num))
				{
					double num3 = 0.0;
					double num4 = 0.0;
					double num5 = 0.0;
					if (Mag < 9.0)
					{
						num3 = 18.4;
						num4 = 33.8;
						num5 = -11.3;
					}
					else if (Mag < 9.5)
					{
						num3 = 14.0;
						num4 = 30.7;
						num5 = -19.4;
					}
					else if (Mag < 10.0)
					{
						num3 = 12.8;
						num4 = 31.4;
						num5 = -11.8;
					}
					else if (Mag < 10.5)
					{
						num3 = 13.6;
						num4 = 35.7;
						num5 = -10.5;
					}
					else if (Mag < 11.0)
					{
						num3 = 16.2;
						num4 = 50.0;
						num5 = 2.1;
					}
					else if (Mag < 11.5)
					{
						num3 = 19.4;
						num4 = 59.9;
						num5 = 0.2;
					}
					else if (Mag < 11.75)
					{
						num3 = 21.8;
						num4 = 64.2;
						num5 = 1.0;
					}
					else if (Mag < 12.0)
					{
						num3 = 17.7;
						num4 = 65.6;
						num5 = -1.9;
					}
					else if (Mag < 12.25)
					{
						num3 = 21.3;
						num4 = 74.8;
						num5 = 2.1;
					}
					else if (Mag < 12.5)
					{
						num3 = 25.7;
						num4 = 73.6;
						num5 = 1.0;
					}
					else if (Mag < 12.75)
					{
						num3 = 27.3;
						num4 = 76.6;
						num5 = 0.5;
					}
					else if (Mag < 13.0)
					{
						num3 = 34.9;
						num4 = 68.9;
						num5 = -2.9;
					}
					double num6 = ((0.0 - Math.Sin(Dec)) * Math.Cos(RA) * num3 - Math.Sin(Dec) * Math.Sin(RA) * num4 + Math.Cos(Dec) * num5) / 1000.0;
					double num7 = (Math.Sin(RA) * num3 - Math.Cos(RA) * num4) / 1000.0;
					double num8 = Utilities.BesselianYear(JD) - 2016.0;
					dRA_asec = (0.0 - num8 * num6) / 1000.0;
					dDec_asec = (0.0 - num8 * num7) / 1000.0;
				}
				break;
			}
		}

		private static string CurrentStarID()
		{
			if (CorrectedStarID.Length > 0)
			{
				return CorrectedStarID;
			}
			if (RecordLength == 48)
			{
				switch (catID)
				{
				case 0:
					return "J" + Utilities.RA_Dec_ToIAUStarID((double)rAmas / 3600000.0, (double)dECmas / 3600000.0, 1);
				case 2:
					return "HIP " + catNumber;
				case 3:
					return "TYC " + (catNumber / 100000u).ToString().PadLeft(4, '0') + "-" + (catNumber % 100000u).ToString().PadLeft(5, '0') + "-" + supNum;
				case 4:
					return "UCAC4 " + (catNumber / 1000000u).ToString().PadLeft(3, '0') + "-" + (catNumber % 1000000u).ToString().PadLeft(6, '0');
				case 5:
				{
					int startIndex = (int)(catNumber / 100000000u);
					string text = ((supNum >= 101) ? ("n" + Math.Abs(supNum - 101)) : ("s" + Math.Abs(supNum - 100)));
					text += "abcd".Substring(startIndex, 1);
					return "Zone-" + catNumber % 10000000u;
				}
				default:
					return "";
				}
			}
			return catID switch
			{
				0 => "J" + Utilities.RA_Dec_ToIAUStarID((double)rAmas / 3600000.0, (double)dECmas / 3600000.0, 1), 
				2 => "HIP " + catNumber, 
				3 => "TYC " + (catNumber / 100000u).ToString().PadLeft(4, '0') + "-" + (catNumber % 100000u).ToString().PadLeft(5, '0') + "-1", 
				4 => "TYC " + (catNumber / 100000u).ToString().PadLeft(4, '0') + "-" + (catNumber % 100000u).ToString().PadLeft(5, '0') + "-2", 
				5 => "TYC " + (catNumber / 100000u).ToString().PadLeft(4, '0') + "-" + (catNumber % 100000u).ToString().PadLeft(5, '0') + "-3", 
				6 => "UCAC4 " + (catNumber / 1000000u).ToString().PadLeft(3, '0') + "-" + (catNumber % 1000000u).ToString().PadLeft(6, '0'), 
				7 => "UCAC4 " + (catNumber / 1000000u).ToString().PadLeft(3, '0') + "-" + (catNumber % 1000000u).ToString().PadLeft(6, '0'), 
				8 => "UCAC4 " + (catNumber / 1000000u).ToString().PadLeft(3, '0') + "-" + (catNumber % 1000000u).ToString().PadLeft(6, '0'), 
				_ => "", 
			};
		}

		internal static void GetGaiaCatalogueForNearbyStars()
		{
			GetAvailableGaiaCatalogues();
			GaiaCatalogueForNearbyStars = GaiaPrimaryFiles[0];
			GaiaCatalogueForNearbyStarsSet = true;
		}

		internal static void GetNearbyStarsFromCoords(string TargetCatID, double RA, double Dec, double MagG, double Epoch20YY_yy, double SepTestArcSec, out double ExtraMagG, out double ExtraMagR, out int BrightStars, out int AllStars, out string AllNearby)
		{
			if (SepTestArcSec < 15.0)
			{
				SepTestArcSec = 15.0;
			}
			double num = SepTestArcSec / 3600.0 / (180.0 / Math.PI);
			bool flag = false;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 30.0;
			double num10 = 30.0;
			double num11 = 20.0;
			double num12 = 20.0;
			string text = "";
			TargetCatID = TargetCatID.Replace("Tycho2 ", "TYC ");
			ExtraMagG = 30.0;
			ExtraMagR = 30.0;
			BrightStars = 0;
			AllStars = 0;
			AllNearby = "";
			GetGaiaCatalogueForNearbyStars();
			if (CurrentRecordLength != 58 || GaiaCatalogueForNearbyStars.Length < 5)
			{
				return;
			}
			MinorPlanetOccultationElements.Near = new List<Nearby>();
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + GaiaCatalogueForNearbyStars + ".inx", FileMode.Open, FileAccess.Read))
			{
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + GaiaCatalogueForNearbyStars + ".bin", FileMode.Open, FileAccess.Read);
				using BinaryReader binaryReader2 = new BinaryReader(fileStream2);
				int num13 = (int)(fileStream2.Length / RecordLength) - 1;
				int num14 = 179 - (int)(360.0 / Math.PI * Dec);
				if (Dec < 0.0)
				{
					num14++;
				}
				int num15 = 361 * num14;
				int num16 = (int)Math.Floor(RA * (180.0 / Math.PI) - 0.006 / Math.Cos(Dec));
				if (num16 < 0)
				{
					num16 = 0;
				}
				int num17 = (int)Math.Ceiling(RA * (180.0 / Math.PI) + 1.006 / Math.Cos(Dec));
				if (num17 > 360)
				{
					num17 = 360;
				}
				fileStream.Seek((num15 + num16) * 4, SeekOrigin.Begin);
				int num18 = binaryReader.ReadInt32() - 1;
				fileStream.Seek((num15 + num17) * 4, SeekOrigin.Begin);
				int num19 = binaryReader.ReadInt32();
				int num20;
				do
				{
					num20 = (num19 + num18) / 2;
					long num21 = num20 * RecordLength;
					fileStream2.Seek(num21, SeekOrigin.Begin);
					double num22 = (double)binaryReader2.ReadInt32() / 3600000.0 / (180.0 / Math.PI);
					fileStream2.Seek(num21 + 5, SeekOrigin.Begin);
					_ = (double)binaryReader2.ReadInt32() / 3600000.0 / (180.0 / Math.PI);
					fileStream2.Seek(num21 + 12, SeekOrigin.Begin);
					double num23 = (double)binaryReader2.ReadInt32() / 3600000000.0 / Math.Cos(Dec_rad) / (180.0 / Math.PI);
					fileStream2.Seek(num21 + 22, SeekOrigin.Begin);
					double num24 = (double)binaryReader2.ReadInt16() / 1000.0;
					if (num22 + (Epoch20YY_yy - num24) * num23 > RA)
					{
						num19 = num20 - 1;
					}
					else
					{
						num18 = num20 + 1;
					}
				}
				while (num19 > num18);
				ReadNext(fileStream2, binaryReader2, num20);
				num2 = Epoch20YY_yy - Epoch_2000;
				num3 = RA;
				num4 = Dec;
				num5 = MagG;
				num6 = num / Math.Cos(num4);
				num8 = 0.0;
				num10 = 30.0;
				num11 = (num12 = 20.0);
				text = "";
				int num25 = -5;
				double PA_atOrigin;
				for (int i = -10; i < 10; i++)
				{
					if (num20 + i < 2)
					{
						continue;
					}
					if (num20 + i > num13)
					{
						break;
					}
					if (ReadNext(fileStream2, binaryReader2, num20 + i))
					{
						num7 = RA_rad + PMRA_rad * num2;
						num8 = Dec_rad + PMDec_rad * num2;
						Utilities.Distance(num3, num4, num7, num8, out num11, out PA_atOrigin);
						num11 *= 648000.0 / Math.PI;
						if (num11 < num12)
						{
							num12 = num11;
							num25 = i;
						}
					}
				}
				int num26 = num20 + num25;
				int num27 = 1;
				do
				{
					num27--;
					if (num20 + num27 < 2 || num26 + num27 > num13)
					{
						break;
					}
					if (!ReadNext(fileStream2, binaryReader2, num26 + num27))
					{
						continue;
					}
					num2 = Epoch20YY_yy - Epoch_2000;
					num7 = RA_rad + PMRA_rad * num2;
					num8 = Dec_rad + PMDec_rad * num2;
					num9 = MagGreen;
					num10 = MagRed;
					text = StarID;
					if (Math.Abs(num7 - num3) > num6)
					{
						break;
					}
					if (!(Math.Abs(num8 - num4) > num))
					{
						Utilities.Distance(num3, num4, num7, num8, out num11, out PA_atOrigin);
						num11 *= 648000.0 / Math.PI;
						if (text == TargetCatID)
						{
							flag = true;
						}
						else if ((num11 < 0.05) & (Math.Abs(num9 - num5) < 0.1))
						{
							flag = true;
						}
						else if (num11 < SepTestArcSec && num9 - num5 < 1.5)
						{
							Nearby nearby = new Nearby();
							nearby.Sep = num11;
							nearby.MagV = num9;
							nearby.MagR = num10;
							nearby.StarID = text;
							MinorPlanetOccultationElements.Near.Add(nearby);
						}
					}
				}
				while (num27 > -500);
				num27 = -1;
				do
				{
					num27++;
					if (!ReadNext(fileStream2, binaryReader2, num26 + num27))
					{
						continue;
					}
					num2 = Epoch20YY_yy - Epoch_2000;
					num7 = RA_rad + PMRA_rad * num2;
					num8 = Dec_rad + PMDec_rad * num2;
					num9 = MagGreen;
					num10 = MagRed;
					text = StarID;
					if (Math.Abs(num7 - num3) > num6)
					{
						break;
					}
					if (!(Math.Abs(num8 - num4) > num))
					{
						Utilities.Distance(num3, num4, num7, num8, out num11, out PA_atOrigin);
						num11 *= 648000.0 / Math.PI;
						if (text == TargetCatID)
						{
							flag = true;
						}
						else if ((num11 < 0.05) & (Math.Abs(num9 - num5) < 0.1))
						{
							flag = true;
						}
						else if (num11 < SepTestArcSec && num9 - num5 < 1.5)
						{
							Nearby nearby = new Nearby();
							nearby.Sep = num11;
							nearby.MagV = num9;
							nearby.MagR = num10;
							nearby.StarID = text;
							MinorPlanetOccultationElements.Near.Add(nearby);
						}
					}
				}
				while (num27 < 500);
			}
			if (MinorPlanetOccultationElements.Near.Count == 0)
			{
				AllNearby = "None within " + SepTestArcSec + "\"";
				return;
			}
			MinorPlanetOccultationElements.Near.Sort();
			if (!flag && ((MinorPlanetOccultationElements.Near[0].Sep < 1.0) & (Math.Abs(MinorPlanetOccultationElements.Near[0].MagV - MagG) < 1.0)))
			{
				MinorPlanetOccultationElements.Near.RemoveAt(0);
				if (MinorPlanetOccultationElements.Near.Count == 0)
				{
					AllNearby = "None within " + SepTestArcSec + "\"";
					return;
				}
			}
			for (int j = 0; j < MinorPlanetOccultationElements.Near.Count; j++)
			{
				if ((MinorPlanetOccultationElements.Near[j].MagV < MagG - 2.0) | ((MinorPlanetOccultationElements.Near[j].MagV < MagG - 1.0) & (MinorPlanetOccultationElements.Near[j].Sep < 4.0)))
				{
					BrightStars++;
				}
				if (MinorPlanetOccultationElements.Near[j].Sep < 4.0)
				{
					if (MinorPlanetOccultationElements.Near[j].MagV < 18.0)
					{
						ExtraMagG = Utilities.CombinedMagnitude(ExtraMagG, MinorPlanetOccultationElements.Near[j].MagV);
					}
					if (MinorPlanetOccultationElements.Near[j].MagR < 18.0)
					{
						ExtraMagR = Utilities.CombinedMagnitude(ExtraMagR, MinorPlanetOccultationElements.Near[j].MagR);
					}
				}
				AllNearby = AllNearby + MinorPlanetOccultationElements.Near[j].ToString() + "\r\n";
			}
			AllStars = MinorPlanetOccultationElements.Near.Count;
		}

		internal static void WriteCurrentAsNextGaiaDR2Record(BinaryWriter GaiaWriteFile)
		{
			GaiaWriteFile.Write(rAmas);
			GaiaWriteFile.Write(rAmuas);
			GaiaWriteFile.Write(dECmas);
			GaiaWriteFile.Write(dECmuas);
			GaiaWriteFile.Write(parallax_masX80);
			GaiaWriteFile.Write(pmRA_muasec_yrCosDec);
			GaiaWriteFile.Write(pmDec_muasec_yr);
			GaiaWriteFile.Write(rV_kms);
			GaiaWriteFile.Write(epoch2000);
			GaiaWriteFile.Write(epochDec_myrs2000);
			GaiaWriteFile.Write(mRed);
			GaiaWriteFile.Write(mGreen);
			GaiaWriteFile.Write(sDev_RA_10muas);
			GaiaWriteFile.Write(sDev_Dec_10muas);
			GaiaWriteFile.Write(sDev_Par_10muas);
			GaiaWriteFile.Write(sDev_pmRA_muas_yr);
			GaiaWriteFile.Write(sDev_pmDec_muas_yr);
			GaiaWriteFile.Write(sDev_RV);
			GaiaWriteFile.Write(spec);
			GaiaWriteFile.Write(catID);
			GaiaWriteFile.Write(catNumber);
			GaiaWriteFile.Write(supNum);
		}

		internal static void WriteCurrentAsNextGaiaDR3Record(BinaryWriter GaiaWriteFile)
		{
			GaiaWriteFile.Write(rAmas);
			GaiaWriteFile.Write(rAmuas);
			GaiaWriteFile.Write(dECmas);
			GaiaWriteFile.Write(dECmuas);
			GaiaWriteFile.Write(parallax_masX80);
			GaiaWriteFile.Write(pmRA_muasec_yrCosDec);
			GaiaWriteFile.Write(pmDec_muasec_yr);
			GaiaWriteFile.Write(rV_kms);
			GaiaWriteFile.Write(epoch2000);
			GaiaWriteFile.Write(mBlue);
			GaiaWriteFile.Write(mGreen);
			GaiaWriteFile.Write(mRed);
			GaiaWriteFile.Write(sDev_RA_10muas);
			GaiaWriteFile.Write(sDev_Dec_10muas);
			GaiaWriteFile.Write(sDev_Par_10muas);
			GaiaWriteFile.Write(sDev_pmRA_muas_yr);
			GaiaWriteFile.Write(sDev_pmDec_muas_yr);
			GaiaWriteFile.Write(sDev_RV);
			GaiaWriteFile.Write(reliability);
			GaiaWriteFile.Write(flags);
			GaiaWriteFile.Write(starDiameter_mas_x5);
			GaiaWriteFile.Write(gaiaVersionOfStar);
			GaiaWriteFile.Write(source_ID);
			GaiaWriteFile.Write(catID);
			GaiaWriteFile.Write(catNumber);
		}

		internal static void ShowGaiaMap()
		{
			try
			{
				((Control)GaiaMap).Show();
			}
			catch
			{
				GaiaMap = new GaiaCatalogueMap();
				((Control)GaiaMap).Show();
			}
		}

		internal static void CreateHipparcosIndexFile(string GaiaFileToBeIndexed)
		{
			int[] array = new int[120404];
			int num = -1;
			using FileStream output = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\Hipparcos_" + GaiaFileToBeIndexed + ".dat", FileMode.Create, FileAccess.Write);
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + GaiaFileToBeIndexed + ".bin", FileMode.Open, FileAccess.Read))
			{
				if (GaiaFileToBeIndexed.Contains("DR3"))
				{
					CurrentRecordLength = 58L;
				}
				else
				{
					CurrentRecordLength = 48L;
				}
				int num2 = (int)((double)fileStream.Length / (double)CurrentRecordLength);
				BinaryReader gaia = new BinaryReader(fileStream);
				PBar pBar = new PBar();
				((Control)pBar).set_Text("Creating Hipparcos Index for " + GaiaFileToBeIndexed);
				pBar.pBarFTP.set_Minimum(0);
				pBar.pBarFTP.set_Maximum(num2);
				((Control)pBar).Show();
				for (num = 0; num < num2; num++)
				{
					if (num % 100 == 0)
					{
						pBar.pBarFTP.set_Value(num);
					}
					ReadNext(fileStream, gaia, num);
					if (catID == 2)
					{
						array[catNumber - 1] = num;
					}
				}
				((Form)pBar).Close();
				((Component)(object)pBar).Dispose();
			}
			BinaryWriter binaryWriter = new BinaryWriter(output);
			for (int i = 0; i < 120404; i++)
			{
				binaryWriter.Write(array[i]);
			}
		}

		internal static bool GetHIPfromGaia(int Hip, out string SourceFile)
		{
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_0096: Invalid comparison between Unknown and I4
			bool result = false;
			SourceFile = "None";
			if (Hip > 118322 || Hip < 1)
			{
				return result;
			}
			GetAvailableGaiaCatalogues();
			if (GaiaPrimaryFiles.Count < 1)
			{
				return false;
			}
			if (GaiaPrimaryFiles[0].Contains("DR3"))
			{
				CurrentRecordLength = 58L;
			}
			else
			{
				CurrentRecordLength = 48L;
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Hipparcos_" + GaiaPrimaryFiles[0] + ".dat") && (int)MessageBox.Show("To access Hipparcos stars from TychoGaia, you need to\r\ncreate an index file. This may take some 10's of seconds.\r\n\r\nDo you want to create the index file?", "Create Hipparcos index", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				CreateHipparcosIndexFile(GaiaPrimaryFiles[0]);
			}
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Hipparcos_" + GaiaPrimaryFiles[0] + ".dat"))
			{
				int recordNumber;
				using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\Hipparcos_" + GaiaPrimaryFiles[0] + ".dat", FileMode.Open, FileAccess.Read))
				{
					BinaryReader binaryReader = new BinaryReader(fileStream);
					fileStream.Seek(4 * (Hip - 1), SeekOrigin.Begin);
					recordNumber = binaryReader.ReadInt32();
				}
				using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + GaiaPrimaryFiles[0] + ".bin", FileMode.Open, FileAccess.Read);
				BinaryReader gaia = new BinaryReader(fileStream2);
				result = ReadNext(fileStream2, gaia, recordNumber);
				if (result)
				{
					SourceFile = GaiaPrimaryFiles[0];
					if (GaiaVersionOfStar == 9)
					{
						SourceFile = "UBSC (+ " + SourceFile + ")";
					}
					return result;
				}
				return false;
			}
			return result;
		}

		internal static bool GetTycho2fromGaiaDR2(int Region, int SeqNum, int Component)
		{
			string SourceFile = "None";
			return GetTycho2fromGaiaDR2(Region, SeqNum, Component, out SourceFile);
		}

		internal static bool GetTycho2fromGaiaDR2(int Region, int SeqNum, int Component, out string SourceFile)
		{
			double num = 0.0;
			double num2 = 0.0;
			int num3 = 3;
			int num4 = 5;
			uint num5 = (uint)(Region * 100000 + SeqNum);
			bool flag = false;
			SourceFile = "None";
			if (Region > 9537 || Region < 1)
			{
				return false;
			}
			FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\GSC Fields.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			fileStream.Seek(16 * (Region - 1), SeekOrigin.Begin);
			double num6 = binaryReader.ReadSingle() - 1f;
			if (num6 < 0.0)
			{
				num6 = 0.0;
			}
			double num7 = binaryReader.ReadSingle();
			num7 += (double)(360 * Convert.ToInt32(num7 == 0.0) + 2);
			if (num7 > 360.0)
			{
				num7 = 360.0;
			}
			double num8 = binaryReader.ReadSingle();
			double num9 = binaryReader.ReadSingle();
			binaryReader.Close();
			int num10 = Convert.ToInt32(179.0 - 2.0 * num8);
			if (num10 < 0)
			{
				num10 = 0;
			}
			if (num10 > 359)
			{
				num10 = 359;
			}
			int num11 = Convert.ToInt32(179.0 - 2.0 * num9) + 1;
			if (num11 < 0)
			{
				num11 = 0;
			}
			if (num11 > 359)
			{
				num11 = 359;
			}
			GetAvailableGaiaCatalogues();
			if (GaiaPrimaryFiles.Count == 0)
			{
				return false;
			}
			string text = GaiaPrimaryFiles[0];
			if (text.Contains("DR3"))
			{
				CurrentRecordLength = 58L;
				num3 = 3;
				num4 = 5;
			}
			else
			{
				CurrentRecordLength = 48L;
				num3 = 3;
				num4 = 3;
			}
			using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			using FileStream fileStream3 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader gaia = new BinaryReader(fileStream3);
			for (int i = num10; i <= num11; i++)
			{
				long num12 = 361 * i;
				fileStream2.Seek(4 * (num12 + (int)num6), SeekOrigin.Begin);
				int num13 = binaryReader2.ReadInt32() - 20;
				if (num13 < 0)
				{
					num13 = 0;
				}
				fileStream2.Seek(4 * (num12 + (int)(num7 + 0.5) + 360 * Convert.ToInt32(num7 == 0.0) + 2), SeekOrigin.Begin);
				int num14 = binaryReader2.ReadInt32() + 20;
				num = (double)(180 - i) / 2.0;
				num2 = num - 0.5;
				for (int j = num13; j <= num14; j++)
				{
					ReadNext(fileStream3, gaia, j);
					if ((gaiaVersionOfStar != 1 || !((Dec_deg > num) | (Dec_deg < num2))) && ((catID >= num3) & (catID <= num4)))
					{
						Utilities.DEGtoDMS(RA_deg / 15.0, 2, 1, MinutesOnly: false);
						Utilities.DEGtoDMS(Dec_deg, 3, 1, MinutesOnly: false);
						if (catNumber == num5 && supNum == Component)
						{
							flag = true;
							SourceFile = text;
							return flag;
						}
					}
				}
				if (flag)
				{
					return flag;
				}
			}
			return flag;
		}

		internal static void CreateUCAC4IndexFile()
		{
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			if (!File.Exists(Settings.Default.UCAC4_Path + "\\u4i\\u4index.unf"))
			{
				MessageBox.Show("To create an index file so that occult can retrieve\r\na UCAC4 star using the UCAC4 identifier, Occult\r\nneeds to access the UCAC4 file\r\nu4index.unf.\r\nThat file is not present, and the index cannot be created.", "UCAC4 index file not available", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			int[,] array = new int[900, 1441];
			using (FileStream input = new FileStream(Settings.Default.UCAC4_Path + "\\u4i\\u4index.unf", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(input);
				for (int i = 1; i <= 1440; i++)
				{
					for (int j = 1; j <= 900; j++)
					{
						array[j - 1, i] = binaryReader.ReadInt32();
					}
				}
			}
			using FileStream output = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx", FileMode.Create, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(output);
			for (int k = 1; k <= 900; k++)
			{
				for (int l = 0; l <= 1440; l += 4)
				{
					binaryWriter.Write(array[k - 1, l]);
				}
			}
		}

		internal static bool Get_U4_fromGaia_DR3_EDR3(int Zone, int NumInZone, out string SourceFile)
		{
			bool flag = false;
			SourceFile = "None";
			int num = 6;
			if (Zone > 900 || Zone < 1)
			{
				return false;
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				http.DownloadUCAC4IndexFile();
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				CreateUCAC4IndexFile();
			}
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx"))
			{
				return false;
			}
			int num2 = 0;
			uint num3 = (uint)(Zone * 1000000 + NumInZone);
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\U4_Gaia14.inx", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				fileStream.Seek(1444 * (Zone - 1) + 4, SeekOrigin.Begin);
				num2 = 0;
				while (binaryReader.ReadInt32() < NumInZone)
				{
					num2++;
					if (num2 >= 360)
					{
						break;
					}
				}
			}
			int num4 = (int)Math.Floor((double)(900 - Zone) / 2.5);
			int num5 = (int)Math.Floor((900.5 - (double)Zone) / 2.5);
			GetAvailableGaiaCatalogues();
			if (GaiaPrimaryFiles.Count == 0)
			{
				return false;
			}
			string text = GaiaPrimaryFiles[0];
			if (text.Contains("DR3"))
			{
				CurrentRecordLength = 58L;
				num = 6;
			}
			else
			{
				CurrentRecordLength = 48L;
				num = 4;
			}
			using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			using FileStream fileStream3 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open, FileAccess.Read);
			BinaryReader gaia = new BinaryReader(fileStream3);
			for (int i = num4; i <= num5; i++)
			{
				long num6 = 361 * i;
				fileStream2.Seek(4 * (num6 + num2 - 1), SeekOrigin.Begin);
				int num7 = binaryReader2.ReadInt32();
				fileStream2.Seek(4 * (num6 + num2 + 1), SeekOrigin.Begin);
				int num8 = binaryReader2.ReadInt32();
				int num9 = num8 + 1;
				for (int j = num7; j <= num8; j++)
				{
					ReadNext(fileStream3, gaia, j);
					if (catID == num)
					{
						Utilities.DEGtoDMS(RA_deg / 15.0, 2, 3, MinutesOnly: false);
						Utilities.DEGtoDMS(Dec_deg, 3, 2, MinutesOnly: false);
						if (catNumber == num3)
						{
							flag = true;
							SourceFile = text;
							break;
						}
					}
					if (j > num9 + 60)
					{
						break;
					}
				}
				if (!flag && num9 < num8)
				{
					ReadNext(fileStream3, gaia, num9);
					flag = true;
					SourceFile = text;
				}
				if (flag)
				{
					return flag;
				}
			}
			return flag;
		}

		internal static bool Get_GaiaStar_fromGaia(double RA_ID_deg, double Dec_ID_deg, out string SourceFile)
		{
			return Get_GaiaStar_fromGaia(RA_ID_deg, Dec_ID_deg, StarCoords_used_as_ID: true, 15.5, 2.0, 10.0, FilterUsingStarMag: false, LimitUsingStarMag: false, out SourceFile);
		}

		internal static bool Get_GaiaStar_fromGaia(double RA_ID_deg, double Dec_ID_deg, bool StarCoords_used_as_ID, double ObservationDate_yrsFrom2000, double MatchDistanceArcSec, double StarMag_ForFilter, bool FilterUsingStarMag, bool LimitUsingStarMag, out string SourceFile)
		{
			double num = 0.0;
			double rA_OfTarget = RA_ID_deg / (180.0 / Math.PI);
			double dec_Target = Dec_ID_deg / (180.0 / Math.PI);
			List<GaiaSepn> list = new List<GaiaSepn>();
			SourceFile = "";
			int num2 = (int)Math.Floor(180.0 - Dec_ID_deg * 2.0);
			int num3 = 0;
			int num4 = 0;
			int num5 = (int)Math.Floor(RA_ID_deg);
			double num6 = MatchDistanceArcSec / 3600.0;
			if (!StarCoords_used_as_ID)
			{
				num6 /= Math.Cos(Dec_ID_deg / (180.0 / Math.PI));
			}
			GetAvailableGaiaCatalogues();
			if (GaiaPrimaryFiles.Count < 1)
			{
				return false;
			}
			string text = ((!LimitUsingStarMag) ? GaiaPrimaryFiles[0] : GetBestGaiaCatalogForStarMag(StarMag_ForFilter));
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".inx", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				if (text.Contains("DR3"))
				{
					CurrentRecordLength = 58L;
				}
				else
				{
					CurrentRecordLength = 48L;
				}
				using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource Files\\Gaia\\" + text + ".bin", FileMode.Open, FileAccess.Read);
				BinaryReader gaia = new BinaryReader(fileStream2);
				long num7 = 361 * num2;
				fileStream.Seek(4 * (num7 + num5), SeekOrigin.Begin);
				num3 = binaryReader.ReadInt32();
				fileStream.Seek(4 * (num7 + num5 + 1), SeekOrigin.Begin);
				num4 = binaryReader.ReadInt32();
				for (int i = num3; i < num4; i++)
				{
					if (!ReadNext(fileStream2, gaia, i))
					{
						continue;
					}
					double rA_OfOrigin = RA_rad + (ObservationDate_yrsFrom2000 - Epoch_2000) * PMRA_rad;
					num = Dec_rad + (ObservationDate_yrsFrom2000 - Epoch_2000) * PMDec_rad;
					Utilities.Distance(rA_OfOrigin, num, rA_OfTarget, dec_Target, out var Distance, out var _);
					Distance *= 648000.0 / Math.PI;
					if (Math.Abs(Distance) < 2.0 * MatchDistanceArcSec)
					{
						GaiaSepn gaiaSepn = new GaiaSepn();
						gaiaSepn.RecNum = i;
						gaiaSepn.Sepn = Distance;
						gaiaSepn.Gmag = MagGreen;
						if (FilterUsingStarMag)
						{
							gaiaSepn.MagDiff = Math.Abs(StarMag_ForFilter - MagGreen);
						}
						else
						{
							gaiaSepn.MagDiff = 0.0;
						}
						list.Add(gaiaSepn);
					}
				}
				if (list.Count > 0)
				{
					list.Sort();
					for (int j = 0; j < list.Count; j++)
					{
						if ((list[j].MagDiff < 2.0) | ((StarMag_ForFilter > 8.0) & (list[j].Gmag > 8.0) & (list[j].MagDiff < 3.0)))
						{
							ReadNext(fileStream2, gaia, list[j].RecNum);
							return true;
						}
					}
				}
			}
			return false;
		}

		internal static void LoadBadHipStars()
		{
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_ED3_BadHipStars.csv"))
			{
				return;
			}
			BadHipStars = new List<BadHipStars>();
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_ED3_BadHipStars.csv"))
			{
				using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_ED3_BadHipStars.csv"))
				{
					do
					{
						BadHipStars badHipStars = new BadHipStars();
						badHipStars.DecodeBadHipLine(streamReader.ReadLine());
						BadHipStars.Add(badHipStars);
					}
					while (!streamReader.EndOfStream);
				}
				BadHipStarsLoaded = true;
			}
			BadHipStarsLoaded_Checked = true;
		}

		internal static bool GetBadHipStar_EntryName(string HipNumber, out string CorrectedStarID)
		{
			CorrectedStarID = "";
			if (!BadHipStarsLoaded)
			{
				if (BadHipStarsLoaded_Checked)
				{
					return false;
				}
				LoadBadHipStars();
				if (!BadHipStarsLoaded)
				{
					return false;
				}
			}
			int.TryParse(HipNumber.Replace("HIP ", ""), out var result);
			if (result > 0)
			{
				int num = 0;
				int num2 = BadHipStars.Count - 1;
				do
				{
					int num3 = (num2 + num) / 2;
					if (result == BadHipStars[num3].Hip)
					{
						CorrectedStarID = BadHipStars[num3].CatID;
						return true;
					}
					if (result > BadHipStars[num3].Hip)
					{
						num = num3 + 1;
					}
					else
					{
						num2 = num3 - 1;
					}
				}
				while (num2 >= num);
			}
			return false;
		}

		internal static bool GetHipNumFromNonHipNumbers(string StarID, out int HipNumber)
		{
			HipNumber = 0;
			if (!BadHipStarsLoaded)
			{
				if (BadHipStarsLoaded_Checked)
				{
					return false;
				}
				LoadBadHipStars();
				if (BadHipStarsLoaded_Checked)
				{
					return false;
				}
			}
			for (int i = 0; i < BadHipStars.Count - 1; i++)
			{
				if (StarID == BadHipStars[i].CatID.ToString())
				{
					HipNumber = BadHipStars[i].Hip;
					return true;
				}
			}
			return false;
		}

		internal static void CreateUBSClist()
		{
			UBSCstars = new List<UBSC>();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\Gaia\\UBSC.txt"))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Gaia\\UBSC.txt"))
			{
				for (int i = 0; i < 57; i++)
				{
					streamReader.ReadLine();
				}
				do
				{
					UBSC uBSC = new UBSC();
					uBSC.DecodeUBSCline(streamReader.ReadLine());
					UBSCstars.Add(uBSC);
				}
				while (!streamReader.EndOfStream);
			}
			UBSCisLoaded = true;
		}

		internal static bool Get_UBSC_Data(int HipNum, ref int RAmas, ref byte RAmuas, ref int Decmas, ref byte Decmuas, ref ushort parallax_masX80, ref int pmRA_muasec_yrCosDec, ref int pmDec_muasec_yr, ref ushort sDev_RA_10muas, ref ushort sDev_Dec_10muas, ref ushort sDev_pmRA_muas_yr, ref ushort sDev_pmDec_muas_yr, ref short Epoch, ref byte GaiaVersion, ref byte flags)
		{
			int num = 0;
			int num2 = UBSCstars.Count - 1;
			do
			{
				int num3 = (num + num2) / 2;
				if (UBSCstars[num3].HipNum == HipNum)
				{
					flags += 64;
					RAmas = UBSCstars[num3].RAmas;
					RAmuas = UBSCstars[num3].RAmuas;
					Decmas = UBSCstars[num3].Decmas;
					Decmuas = UBSCstars[num3].Decmuas;
					if (Parallax_asec == 0.0)
					{
						parallax_masX80 = UBSCstars[num3].Parallax_masX80;
					}
					pmRA_muasec_yrCosDec = UBSCstars[num3].pmRA_muasec_yrCosDec;
					pmDec_muasec_yr = UBSCstars[num3].pmDec_muasec_yr;
					sDev_RA_10muas = UBSCstars[num3].sDev_RA_10muas;
					sDev_Dec_10muas = UBSCstars[num3].sDev_Dec_10muas;
					sDev_pmRA_muas_yr = UBSCstars[num3].sDev_pmRA_muas_yr;
					sDev_pmDec_muas_yr = UBSCstars[num3].sDev_pmDec_muas_yr;
					Epoch = UBSCstars[num3].epoch2000;
					GaiaVersion = UBSCstars[num3].GaiaVersion;
					if (NoGaiaProperMotion == 1)
					{
						flags -= 2;
					}
					return true;
				}
				if (UBSCstars[num3].HipNum > HipNum)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			return false;
		}
	}
}

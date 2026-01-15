using System;
using System.IO;
using System.Text;
using Occult.Properties;

namespace Occult
{
	internal class PPMXL
	{
		private const double radian = 180.0 / Math.PI;

		private const double mas_in_degree = 3600000.0;

		private const double mas_in_radian = 648000000.0 / Math.PI;

		private const double tenth_mas_in_radian = 2062648062.4709637;

		private static string PPMXLpath;

		private static string Path;

		private static FileStream StarCatFileStream;

		private static FileStream IndexFileStream;

		private static BinaryReader PPMXLFile;

		private const int PPMXLRecordLength = 34;

		private static StringBuilder N;

		private static int ra_J2000 = 0;

		private static int pm_RA = 0;

		private static int dec_J2000 = 0;

		private static int pm_Dec = 0;

		private static short e_RA = 0;

		private static short e_Dec = 0;

		private static short e_PMRA = 0;

		private static short e_PMDec = 0;

		private static short epoch_PMRA = -16000;

		private static short epoch_PMDec = -16000;

		private static short mB = -900;

		private static short mV = -900;

		private static short mR = -900;

		private static int CurrentZone;

		private static int PPMXLSeqNumber;

		public static double RA_J2000
		{
			get
			{
				return (double)ra_J2000 / (648000000.0 / Math.PI);
			}
			set
			{
				ra_J2000 = (int)(value * (648000000.0 / Math.PI));
			}
		}

		public static double RA_J2000_mas => ra_J2000;

		public static double PM_RA
		{
			get
			{
				return (double)pm_RA / 2062648062.4709637 / Math.Cos((double)dec_J2000 / (648000000.0 / Math.PI));
			}
			set
			{
				pm_RA = (int)(value * 2062648062.4709637);
			}
		}

		public static double Dec_J2000
		{
			get
			{
				return (double)dec_J2000 / (648000000.0 / Math.PI);
			}
			set
			{
				dec_J2000 = (int)(value * (648000000.0 / Math.PI));
			}
		}

		public static double Dec_J2000_mas => dec_J2000;

		public static double PM_Dec
		{
			get
			{
				return (double)pm_Dec / 2062648062.4709637;
			}
			set
			{
				pm_Dec = (int)(value * 2062648062.4709637);
			}
		}

		public static double Error_RA
		{
			get
			{
				return (double)e_RA / (648000000.0 / Math.PI);
			}
			set
			{
				e_RA = (short)(value * (648000000.0 / Math.PI));
			}
		}

		public static double Error_Dec
		{
			get
			{
				return (double)e_Dec / (648000000.0 / Math.PI);
			}
			set
			{
				e_Dec = (short)(value * (648000000.0 / Math.PI));
			}
		}

		public static double Error_PMRA
		{
			get
			{
				return (double)e_PMRA / 2062648062.4709637;
			}
			set
			{
				e_PMRA = (short)(value * 2062648062.4709637);
			}
		}

		public static double Error_PMDec
		{
			get
			{
				return (double)e_PMDec / 2062648062.4709637;
			}
			set
			{
				e_PMDec = (short)(value * 2062648062.4709637);
			}
		}

		public static double Epoch_PMRA_ref2000
		{
			get
			{
				return (double)epoch_PMRA / 100.0;
			}
			set
			{
				epoch_PMRA = (short)(value * 100.0);
			}
		}

		public static double Epoch_PMDec_ref2000
		{
			get
			{
				return (double)epoch_PMDec / 100.0;
			}
			set
			{
				epoch_PMDec = (short)(value * 100.0);
			}
		}

		public static double mag_B
		{
			get
			{
				if (mB > -500)
				{
					return (double)mB / 100.0;
				}
				return 25.0;
			}
			set
			{
				mB = (short)(value * 100.0);
			}
		}

		public static double mag_V
		{
			get
			{
				if (mV > -500)
				{
					return (double)mV / 100.0;
				}
				return 25.0;
			}
			set
			{
				mV = (short)(value * 100.0);
			}
		}

		public static double mag_R
		{
			get
			{
				if (mR > -500)
				{
					return (double)mR / 100.0;
				}
				return 25.0;
			}
			set
			{
				mR = (short)(value * 100.0);
			}
		}

		public static double mag
		{
			get
			{
				if (mV > -500)
				{
					return (double)mV / 100.0;
				}
				if ((mR > -500) & (mB > -500))
				{
					return ((double)mR + 0.54 * (double)mB) / 154.0;
				}
				if (mR > -500)
				{
					return (double)mR / 100.0;
				}
				if (mB > -500)
				{
					return (double)mB / 100.0;
				}
				return 30.0;
			}
		}

		public static string PPMXnumber => DecZone_to_PPMXLFile(CurrentZone) + "-" + PPMXLSeqNumber.ToString().PadLeft(7, '0');

		public string IAUidentifier
		{
			get
			{
				double num = (double)ra_J2000 / 3600000.0 / 15.0;
				int num2 = (int)Math.Floor(num);
				double num3 = 60.0 * (num - (double)num2);
				int num4 = (int)Math.Floor(num3);
				double num5 = 60.0 * (num3 - (double)num4);
				string text = num2.ToString().PadLeft(2, '0') + num4.ToString().PadLeft(2, '0') + string.Format("{0,2:F1}", num5).PadLeft(4, '0');
				text = ((!(Dec_J2000 < 0.0)) ? (text + "+") : (text + "-"));
				num = Math.Abs((double)dec_J2000 / 3600000.0);
				num2 = (int)Math.Floor(num);
				num3 = 60.0 * (num - (double)num2);
				num4 = (int)Math.Floor(num3);
				num5 = 60.0 * (num3 - (double)num4);
				return text + num2.ToString().PadLeft(2, '0') + num4.ToString().PadLeft(2, '0') + string.Format("{0,1:F0}", num5).PadLeft(2, '0');
			}
		}

		public static string PPMXL_ASCII_line
		{
			get
			{
				N = new StringBuilder();
				N.Append((DecZone_to_PPMXLFile(CurrentZone) + "-" + PPMXLSeqNumber.ToString().PadLeft(7, '0')).PadRight(14));
				if (mV < -900)
				{
					N.Append("  --- ");
				}
				else
				{
					N.AppendFormat(" {0,5:f2}", mag_V);
				}
				if (mB < -900)
				{
					N.Append("  --- ");
				}
				else
				{
					N.AppendFormat(" {0,5:f2}", mag_B);
				}
				if (mR < -900)
				{
					N.Append("  ---     ");
				}
				else
				{
					N.AppendFormat(" {0,5:f2}    ", mag_R);
				}
				N.Append(Utilities.DEGtoDMS((double)ra_J2000 / 3600000.0 / 15.0, 2, 4, MinutesOnly: false));
				N.AppendFormat(" {0,8:F5} ", (double)pm_RA / Math.Cos((double)dec_J2000 / (648000000.0 / Math.PI)) / 150000.0);
				N.AppendFormat(" {0,6:F3}", (double)e_RA / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)e_PMRA / 10000.0);
				N.AppendFormat(" {0,7:F2}    ", (double)epoch_PMRA / 100.0 + 2000.0);
				N.Append(Utilities.DEGtoDMS((double)dec_J2000 / 3600000.0, 3, 3, MinutesOnly: false));
				N.AppendFormat(" {0,8:F4} ", (double)pm_Dec / 10000.0);
				N.AppendFormat(" {0,6:F3}", (double)e_Dec / 1000.0);
				N.AppendFormat(" {0,7:F4}", (double)e_PMDec / 10000.0);
				N.AppendFormat(" {0,7:F2}", (double)epoch_PMDec / 100.0 + 2000.0);
				return N.ToString();
			}
		}

		public static double ErrorRA_arcsecs(double T)
		{
			double num = (double)e_RA / 1000.0;
			double num2 = (double)e_PMRA / 10000.0 * (T - 2000.0 - (double)epoch_PMRA / 100.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static double ErrorDec_arcsecs(double T)
		{
			double num = (double)e_Dec / 1000.0;
			double num2 = (double)e_PMDec / 10000.0 * (T - 2000.0 - (double)epoch_PMDec / 100.0);
			return Math.Sqrt(num * num + num2 * num2);
		}

		public static void InitialisePPMXL()
		{
			PPMXLpath = Settings.Default.PPMXL_Path;
		}

		public static int PPMXLFile_to_DecZone(string FileBase)
		{
			int num = "ns".IndexOf(FileBase.Substring(0, 1));
			int num2 = int.Parse(FileBase.Substring(1, 2));
			"abcd".IndexOf(FileBase.Substring(3, 1));
			if (num == 1)
			{
				return 4 * (90 - num2) - num + 1;
			}
			return 4 * (90 + num2) + num;
		}

		public static string DecZone_to_PPMXLFile(int DecZone)
		{
			string text = "n";
			int num;
			if (DecZone < 361)
			{
				text = "s";
				num = 360 - DecZone;
			}
			else
			{
				num = DecZone - 361;
			}
			int num2 = num / 4;
			int startIndex = num % 4;
			return text + num2.ToString().PadLeft(2, '0') + "abcd".Substring(startIndex, 1);
		}

		public static void Open_PPMXL_Catalogue(int DecZone)
		{
			if (DecZone < 721)
			{
				CurrentZone = DecZone;
				Path = PPMXLpath + "\\" + DecZone_to_PPMXLFile(DecZone) + ".dat";
				StarCatFileStream = new FileStream(Path, FileMode.Open, FileAccess.Read);
				PPMXLFile = new BinaryReader(StarCatFileStream);
			}
		}

		public static void Open_PPMXL_Catalogue(string Zone)
		{
			Path = PPMXLpath + "\\" + Zone + ".dat";
			StarCatFileStream = new FileStream(Path, FileMode.Open, FileAccess.Read);
			PPMXLFile = new BinaryReader(StarCatFileStream);
		}

		public static int GetPPMXLIndexValue(int DecZone, int RAstep1800)
		{
			using (IndexFileStream = new FileStream(PPMXLpath + "\\" + DecZone_to_PPMXLFile(DecZone) + ".inx", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(IndexFileStream);
				IndexFileStream.Seek(4 * RAstep1800, SeekOrigin.Begin);
				return binaryReader.ReadInt32();
			}
		}

		internal static void Read_PPMXL_entry(int Record)
		{
			PPMXLSeqNumber = Record + 1;
			StarCatFileStream.Seek(34 * Record, SeekOrigin.Begin);
			ra_J2000 = PPMXLFile.ReadInt32();
			dec_J2000 = PPMXLFile.ReadInt32();
			mB = PPMXLFile.ReadInt16();
			mR = PPMXLFile.ReadInt16();
			mV = PPMXLFile.ReadInt16();
			pm_RA = PPMXLFile.ReadInt32();
			pm_Dec = PPMXLFile.ReadInt32();
			e_RA = PPMXLFile.ReadInt16();
			e_Dec = PPMXLFile.ReadInt16();
			e_PMRA = PPMXLFile.ReadInt16();
			e_PMDec = PPMXLFile.ReadInt16();
			epoch_PMRA = PPMXLFile.ReadInt16();
			epoch_PMDec = PPMXLFile.ReadInt16();
		}

		internal static bool Read_PPMXL_Number(string Zone, int PPMXLnum)
		{
			InitialisePPMXL();
			Open_PPMXL_Catalogue(Zone);
			Read_PPMXL_entry(PPMXLnum - 1);
			Close_PPMXL_Catalogue();
			ReleasePPMXL();
			return true;
		}

		internal static string PPMXLnumFromCoords(double RAstar, double DecStar, double PMyears, out double Mb, out double Mv, out double Mr)
		{
			string result = "";
			double num = 4.84813681109536E-06;
			double num2 = num / Math.Cos(DecStar);
			double num3 = 1.0;
			Mb = (Mv = (Mr = 25.0));
			int decZone = (int)(361.0 + 720.0 / Math.PI * DecStar);
			int num4 = (int)Math.Floor(RAstar * (180.0 / Math.PI) * 5.0);
			Open_PPMXL_Catalogue(decZone);
			int pPMXLIndexValue = GetPPMXLIndexValue(decZone, num4);
			int num5 = GetPPMXLIndexValue(decZone, num4 + 1) - pPMXLIndexValue;
			for (int i = 0; i < num5; i++)
			{
				Read_PPMXL_entry(pPMXLIndexValue + i);
				num3 = 1.0;
				if ((PM_RA == 0.0) & (PM_Dec == 0.0))
				{
					num3 = 2.0;
				}
				if ((Math.Abs(RA_J2000 + PM_RA * PMyears - RAstar) < num3 * num2) & (Math.Abs(Dec_J2000 + PM_Dec * PMyears - DecStar) < num3 * num))
				{
					result = PPMXnumber;
					Mb = mag_B;
					Mv = mag_V;
					Mr = mag_R;
					break;
				}
				if (RA_J2000 - RAstar > 4E-05)
				{
					break;
				}
			}
			Close_PPMXL_Catalogue();
			return result;
		}

		public static void Close_PPMXL_Catalogue()
		{
			PPMXLFile.Close();
		}

		public static void ReleasePPMXL()
		{
		}
	}
}

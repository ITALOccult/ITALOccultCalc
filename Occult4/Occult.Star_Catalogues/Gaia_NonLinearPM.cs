using System;
using System.Collections.Generic;
using System.IO;

namespace Occult.Star_Catalogues
{
	internal class Gaia_NonLinearPM
	{
		private static List<Gaia_NonLinearPM> Gaia_NonLinear = new List<Gaia_NonLinearPM>();

		private static Gaia_NonLinearPM NonLinear;

		private static bool CantUseNonlinear = false;

		private ulong gaiaID;

		private double ra;

		private double dec;

		private double pmRA;

		private double dpmRA;

		private double d2pmRA;

		private double pmDec;

		private double dpmDec;

		private double d2pmDec;

		internal ulong GaiaID
		{
			get
			{
				return gaiaID;
			}
			set
			{
				gaiaID = value;
			}
		}

		internal double RA
		{
			get
			{
				return ra;
			}
			set
			{
				ra = value;
			}
		}

		internal double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		internal double PMRA
		{
			get
			{
				return pmRA;
			}
			set
			{
				pmRA = value;
			}
		}

		internal double DpmRA
		{
			get
			{
				return dpmRA;
			}
			set
			{
				dpmRA = value;
			}
		}

		internal double D2pmRA
		{
			get
			{
				return d2pmRA;
			}
			set
			{
				d2pmRA = value;
			}
		}

		internal double PMDec
		{
			get
			{
				return pmDec;
			}
			set
			{
				pmDec = value;
			}
		}

		internal double DpmDec
		{
			get
			{
				return dpmDec;
			}
			set
			{
				dpmDec = value;
			}
		}

		internal double D2pmDec
		{
			get
			{
				return d2pmDec;
			}
			set
			{
				d2pmDec = value;
			}
		}

		internal static void Fill_Gaia_NonLinear()
		{
			Gaia_NonLinear.Clear();
			try
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_NonLinear.csv");
				string[] array = streamReader.ReadLine()!.Split();
				do
				{
					array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					NonLinear = new Gaia_NonLinearPM();
					NonLinear.RA = double.Parse(array[0]);
					NonLinear.Dec = double.Parse(array[1]);
					NonLinear.PMRA = double.Parse(array[2]);
					NonLinear.DpmRA = double.Parse(array[3]);
					NonLinear.D2pmRA = double.Parse(array[4]);
					NonLinear.pmDec = double.Parse(array[5]);
					NonLinear.DpmDec = double.Parse(array[6]);
					NonLinear.d2pmDec = double.Parse(array[7]);
					Gaia_NonLinear.Add(NonLinear);
				}
				while (!streamReader.EndOfStream);
			}
			catch
			{
				CantUseNonlinear = true;
			}
		}

		internal static bool GaiaMeanPMs(double RA, double Dec, double YearsFromEpoch, out double MeanPMra_rad, out double MeanPMdec_rad)
		{
			MeanPMra_rad = (MeanPMdec_rad = 0.0);
			double num = 0.0;
			double num2 = 0.0;
			if (CantUseNonlinear)
			{
				return false;
			}
			if (Gaia_NonLinear.Count == 0)
			{
				CantUseNonlinear = false;
				Fill_Gaia_NonLinear();
			}
			int num3 = 1;
			int num4 = Gaia_NonLinear.Count - 1;
			int num5;
			do
			{
				num5 = (num3 + num4) / 2;
				if (Gaia_NonLinear[num5].RA == RA)
				{
					break;
				}
				if (Gaia_NonLinear[num5].RA < RA)
				{
					num3 = num5 + 1;
				}
				else
				{
					num4 = num5 - 1;
				}
			}
			while (num3 <= num4);
			for (int i = num4 - 2; i <= num3 + 2; i++)
			{
				if (!((i < 1) | (i >= Gaia_NonLinear.Count)) && ((Math.Abs(Gaia_NonLinear[num5].RA - RA) < 5E-05) & (Math.Abs(Gaia_NonLinear[num5].Dec - Dec) < 5E-05)))
				{
					num = (Gaia_NonLinear[num5].PMRA * YearsFromEpoch + Gaia_NonLinear[num5].DpmRA * YearsFromEpoch * YearsFromEpoch + Gaia_NonLinear[num5].D2pmRA * YearsFromEpoch * YearsFromEpoch * YearsFromEpoch) / YearsFromEpoch;
					num2 = (Gaia_NonLinear[num5].PMDec * YearsFromEpoch + Gaia_NonLinear[num5].DpmDec * YearsFromEpoch * YearsFromEpoch + Gaia_NonLinear[num5].D2pmDec * YearsFromEpoch * YearsFromEpoch * YearsFromEpoch) / YearsFromEpoch;
					MeanPMra_rad = num / (180.0 / Math.PI) / 3600000.0;
					MeanPMdec_rad = num2 / (180.0 / Math.PI) / 3600000.0;
					return true;
				}
			}
			return false;
		}
	}
}

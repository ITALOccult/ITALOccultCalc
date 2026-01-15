using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Occult
{
	internal class MagValues : IComparable
	{
		internal static List<MagValues> ColorMagnitudeData = new List<MagValues>();

		internal static bool AsteroidMagnitudeFileExists = true;

		private static string MagnitudeFile = Utilities.AppPath + "\\Resource Files\\HGsHG1G2_Magnitudes.csv";

		private int asteroid;

		private double h0green;

		private double h0red;

		private double h0greenUncert;

		private double h0redUncert;

		private double phaseSlopeGreen;

		private double phaseSlopeRed;

		private double sHG1G2green;

		private double sHG1G2red;

		private double sHG1G2greenUncert;

		private double sHG1G2redUncert;

		private double sHG1G2_G1green;

		private double sHG1G2_G1red;

		private double sHG1G2_G2green;

		private double sHG1G2_G2red;

		private double sHG1G2_R;

		private double sHG1G2_alpha;

		private double sHG1G2_delta;

		internal int Asteroid
		{
			get
			{
				return asteroid;
			}
			set
			{
				asteroid = value;
			}
		}

		internal double H0Green
		{
			get
			{
				return h0green;
			}
			set
			{
				h0green = value;
			}
		}

		internal double H0GreenUncert
		{
			get
			{
				return h0greenUncert;
			}
			set
			{
				h0greenUncert = value;
			}
		}

		internal double PhaseSlopeGreen
		{
			get
			{
				return phaseSlopeGreen;
			}
			set
			{
				phaseSlopeGreen = value;
			}
		}

		internal double H0Red
		{
			get
			{
				return h0red;
			}
			set
			{
				h0red = value;
			}
		}

		internal double H0RedUncert
		{
			get
			{
				return h0redUncert;
			}
			set
			{
				h0redUncert = value;
			}
		}

		internal double PhaseSlopeRed
		{
			get
			{
				return phaseSlopeRed;
			}
			set
			{
				phaseSlopeRed = value;
			}
		}

		internal double SHG1G2Green
		{
			get
			{
				return sHG1G2green;
			}
			set
			{
				sHG1G2green = value;
			}
		}

		internal double SHG1G2GreenUncert
		{
			get
			{
				return sHG1G2greenUncert;
			}
			set
			{
				sHG1G2greenUncert = value;
			}
		}

		internal double SHG1G2_G1Green
		{
			get
			{
				return sHG1G2_G1green;
			}
			set
			{
				sHG1G2_G1green = value;
			}
		}

		internal double SHG1G2_G2Green
		{
			get
			{
				return sHG1G2_G2green;
			}
			set
			{
				sHG1G2_G2green = value;
			}
		}

		internal double SHG1G2Red
		{
			get
			{
				return sHG1G2red;
			}
			set
			{
				sHG1G2red = value;
			}
		}

		internal double SHG1G2RedUncert
		{
			get
			{
				return sHG1G2redUncert;
			}
			set
			{
				sHG1G2redUncert = value;
			}
		}

		internal double SHG1G2_G1Red
		{
			get
			{
				return sHG1G2_G1red;
			}
			set
			{
				sHG1G2_G1red = value;
			}
		}

		internal double SHG1G2_G2Red
		{
			get
			{
				return sHG1G2_G2red;
			}
			set
			{
				sHG1G2_G2red = value;
			}
		}

		internal double SHG1G2_R
		{
			get
			{
				return sHG1G2_R;
			}
			set
			{
				sHG1G2_R = value;
			}
		}

		internal double SHG1G2_alpha
		{
			get
			{
				return sHG1G2_alpha;
			}
			set
			{
				sHG1G2_alpha = value;
			}
		}

		internal double SHG1G2_delta
		{
			get
			{
				return sHG1G2_delta;
			}
			set
			{
				sHG1G2_delta = value;
			}
		}

		internal static bool ReadMagnitudeFile()
		{
			if (!File.Exists(MagnitudeFile))
			{
				http.GetAsteroidMagnitudeFile();
			}
			if (!File.Exists(MagnitudeFile))
			{
				return false;
			}
			ColorMagnitudeData.Clear();
			using StreamReader streamReader = new StreamReader(MagnitudeFile);
			do
			{
				MagValues magValues = new MagValues();
				magValues.DecodeInputLine(streamReader.ReadLine());
				ColorMagnitudeData.Add(magValues);
			}
			while (!streamReader.EndOfStream);
			return true;
		}

		internal static int GetColorMagnitudeRecord(int AsteroidNum)
		{
			if (!AsteroidMagnitudeFileExists)
			{
				return -1;
			}
			if (ColorMagnitudeData.Count < 100)
			{
				AsteroidMagnitudeFileExists = ReadMagnitudeFile();
				if (!AsteroidMagnitudeFileExists)
				{
					return -1;
				}
			}
			int num = 0;
			int num2 = ColorMagnitudeData.Count - 1;
			do
			{
				int num3 = (num2 + num) / 2;
				if (AsteroidNum == ColorMagnitudeData[num3].Asteroid)
				{
					return num3;
				}
				if (AsteroidNum < ColorMagnitudeData[num3].Asteroid)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			return -1;
		}

		internal bool GetColorMagnitudes(double RA, double Dec, double R_Sun_Asteroid, double Delta, double PhaseAngle, out double MagV, out double MagR, out double MagV_Uncert, out double MagR_Uncert)
		{
			if (PhaseAngle < 0.0)
			{
				PhaseAngle += Math.PI;
			}
			double num3;
			double num4;
			if (sHG1G2_G1green > 0.0)
			{
				double num = Math.Abs(Math.Sin(Dec) * Math.Sin(sHG1G2_delta / (180.0 / Math.PI)) + Math.Cos(Dec) * Math.Cos(sHG1G2_delta / (180.0 / Math.PI)) * Math.Cos(RA - sHG1G2_alpha / (180.0 / Math.PI)));
				double num2 = 2.5 * Math.Log10(1.0 - (1.0 - sHG1G2_R) * num);
				GetPhi(PhaseAngle * (180.0 / Math.PI), out var phi_, out var phi_2, out var phi_3);
				num3 = sHG1G2green + 5.0 * Math.Log10(Delta * R_Sun_Asteroid);
				num3 += -2.5 * Math.Log10(sHG1G2_G1green * phi_ + sHG1G2_G2green * phi_2 + (1.0 - sHG1G2_G1green - sHG1G2_G2green) * phi_3);
				num3 += num2;
				MagV_Uncert = sHG1G2greenUncert;
				num4 = sHG1G2red + 5.0 * Math.Log10(Delta * R_Sun_Asteroid);
				num4 += -2.5 * Math.Log10(sHG1G2_G1red * phi_ + sHG1G2_G2red * phi_2 + (1.0 - sHG1G2_G1red - sHG1G2_G2red) * phi_3);
				num4 += num2;
				MagR_Uncert = sHG1G2redUncert;
			}
			else
			{
				num3 = h0green + 5.0 * Math.Log10(Delta * R_Sun_Asteroid);
				num3 += -2.5 * Math.Log10((1.0 - PhaseSlopeGreen) * Math.Pow(Math.E, -3.33 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 0.63)) + PhaseSlopeGreen * Math.Pow(Math.E, -1.87 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 1.22)));
				MagV_Uncert = h0greenUncert;
				num4 = h0red + 5.0 * Math.Log10(Delta * R_Sun_Asteroid);
				num4 += -2.5 * Math.Log10((1.0 - PhaseSlopeRed) * Math.Pow(Math.E, -3.33 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 0.63)) + PhaseSlopeRed * Math.Pow(Math.E, -1.87 * Math.Pow(Math.Tan(PhaseAngle / 2.0), 1.22)));
				MagR_Uncert = h0redUncert;
			}
			MagV = num3 - 0.03 - 0.42 * (num3 - num4);
			MagR = num4 - 0.51 - 0.15 * (num3 - num4);
			return true;
		}

		internal static void GetPhi(double PhaseAngleDeg, out double phi_1, out double phi_2, out double phi_3)
		{
			if (PhaseAngleDeg < 30.0)
			{
				phi_1 = 1.0 - 0.038074 * PhaseAngleDeg + 0.00066616 * PhaseAngleDeg * PhaseAngleDeg - 4.5352E-06 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
				phi_2 = 1.0 - 0.008615 * PhaseAngleDeg - 0.00020448 * PhaseAngleDeg * PhaseAngleDeg + 2.6416E-06 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 60.0)
			{
				phi_1 = 0.96472 - 0.031607 * PhaseAngleDeg + 0.00041137 * PhaseAngleDeg * PhaseAngleDeg - 1.9219E-06 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
				phi_2 = 1.03336 - 0.01473 * PhaseAngleDeg + 3.642E-05 * PhaseAngleDeg * PhaseAngleDeg + 1.708E-07 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 90.0)
			{
				phi_1 = 0.71776 - 0.016515 * PhaseAngleDeg + 0.00013697 * PhaseAngleDeg * PhaseAngleDeg - 3.975E-07 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
				phi_2 = 1.09632 - 0.018578 * PhaseAngleDeg + 0.00010638 * PhaseAngleDeg * PhaseAngleDeg - 2.179E-07 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 120.0)
			{
				phi_1 = 0.62639 - 0.013215 * PhaseAngleDeg + 9.889E-05 * PhaseAngleDeg * PhaseAngleDeg - 2.565E-07 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
				phi_2 = 0.96722 - 0.013916 * PhaseAngleDeg + 5.259E-05 * PhaseAngleDeg * PhaseAngleDeg - 1.86E-08 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else
			{
				phi_1 = 0.18717 - 0.001746 * PhaseAngleDeg + 1.29E-06 * PhaseAngleDeg * PhaseAngleDeg + 1.46E-08 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
				phi_2 = 1.54239 - 0.028934 * PhaseAngleDeg + 0.00018041 * PhaseAngleDeg * PhaseAngleDeg - 3.737E-07 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			if (PhaseAngleDeg < 1.0)
			{
				phi_3 = 1.0 - 0.619807 * PhaseAngleDeg + 0.22905703 * PhaseAngleDeg * PhaseAngleDeg - 0.0318958034 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 2.0)
			{
				phi_3 = 0.99764 - 0.608391 * PhaseAngleDeg + 0.21606675 * PhaseAngleDeg * PhaseAngleDeg - 0.0279593538 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 4.0)
			{
				phi_3 = 0.78526 - 0.236734 * PhaseAngleDeg + 0.03023794 * PhaseAngleDeg * PhaseAngleDeg - 0.0014123819 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 8.0)
			{
				phi_3 = 0.74454 - 0.201102 * PhaseAngleDeg + 0.02133012 * PhaseAngleDeg * PhaseAngleDeg - 0.000776109 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 12.0)
			{
				phi_3 = 0.50193 - 0.089906 * PhaseAngleDeg + 0.00616705 * PhaseAngleDeg * PhaseAngleDeg - 0.0001443141 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else if (PhaseAngleDeg < 30.0)
			{
				phi_3 = 0.23943 - 0.022094 * PhaseAngleDeg + 0.00069832 * PhaseAngleDeg * PhaseAngleDeg - 7.5961E-06 * PhaseAngleDeg * PhaseAngleDeg * PhaseAngleDeg;
			}
			else
			{
				phi_3 = 0.0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Asteroid + ",");
			stringBuilder.AppendFormat("{0,1:f2},", H0Green);
			stringBuilder.AppendFormat("{0,1:f1},", H0GreenUncert);
			stringBuilder.AppendFormat("{0,1:f2},", PhaseSlopeGreen);
			stringBuilder.AppendFormat("{0,1:f2},", H0Red);
			stringBuilder.AppendFormat("{0,1:f1},", H0RedUncert);
			stringBuilder.AppendFormat("{0,1:f2}", PhaseSlopeRed);
			if ((SHG1G2GreenUncert < 0.35) & (SHG1G2GreenUncert > 0.0))
			{
				stringBuilder.AppendFormat(",{0,1:f2},", SHG1G2Green);
				stringBuilder.AppendFormat("{0,1:f1},", SHG1G2GreenUncert);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2_G1Green);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2_G2Green);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2Red);
				stringBuilder.AppendFormat("{0,1:f1},", SHG1G2RedUncert);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2_G1Red);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2_G2Red);
				stringBuilder.AppendFormat("{0,1:f2},", SHG1G2_R);
				stringBuilder.AppendFormat("{0,1:f0},", SHG1G2_alpha);
				stringBuilder.AppendFormat("{0,1:f0}", SHG1G2_delta);
			}
			return stringBuilder.ToString();
		}

		internal void DecodeInputLine(string X)
		{
			string[] array = X.Split(new char[1] { ',' });
			int.TryParse(array[0], out asteroid);
			double.TryParse(array[1], out h0green);
			double.TryParse(array[2], out h0greenUncert);
			double.TryParse(array[3], out phaseSlopeGreen);
			double.TryParse(array[4], out h0red);
			double.TryParse(array[5], out h0redUncert);
			double.TryParse(array[6], out phaseSlopeRed);
			if (array.Length > 7)
			{
				double.TryParse(array[7], out sHG1G2green);
				double.TryParse(array[8], out sHG1G2greenUncert);
				double.TryParse(array[9], out sHG1G2_G1green);
				double.TryParse(array[10], out sHG1G2_G2green);
				double.TryParse(array[11], out sHG1G2red);
				double.TryParse(array[12], out sHG1G2redUncert);
				double.TryParse(array[13], out sHG1G2_G1red);
				double.TryParse(array[14], out sHG1G2_G2red);
				double.TryParse(array[15], out sHG1G2_R);
				double.TryParse(array[16], out sHG1G2_alpha);
				double.TryParse(array[17], out sHG1G2_delta);
			}
		}

		public int CompareTo(object other)
		{
			return Asteroid.CompareTo(((MagValues)other).Asteroid);
		}
	}
}

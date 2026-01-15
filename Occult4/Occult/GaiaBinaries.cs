using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Occult
{
	internal class GaiaBinaries
	{
		internal static List<GaiaBinaries> GaiaBinaryData = new List<GaiaBinaries>();

		internal static bool GaiaBinaryFileExists = true;

		private static string GaiaBinaryFile = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia_Binaries.csv";

		private int asteroid;

		private double period;

		private double sep1;

		private double sep1Uncert;

		private double diaRatio1;

		private double sep2;

		private double sep2Uncert;

		private double diaRatio2;

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

		internal double Period
		{
			get
			{
				return period;
			}
			set
			{
				period = value;
			}
		}

		internal double Sepn1
		{
			get
			{
				return sep1;
			}
			set
			{
				sep1 = value;
			}
		}

		internal double Sepn1_Uncert
		{
			get
			{
				return sep1Uncert;
			}
			set
			{
				sep1Uncert = value;
			}
		}

		internal double DiaRatio1
		{
			get
			{
				return diaRatio1;
			}
			set
			{
				diaRatio1 = value;
			}
		}

		internal double Sepn2
		{
			get
			{
				return sep2;
			}
			set
			{
				sep2 = value;
			}
		}

		internal double Sepn2_Uncert
		{
			get
			{
				return sep2Uncert;
			}
			set
			{
				sep2Uncert = value;
			}
		}

		internal double DiaRatio2
		{
			get
			{
				return diaRatio2;
			}
			set
			{
				diaRatio2 = value;
			}
		}

		internal static int GetGaiaBinary(int AsteroidNum)
		{
			if (AsteroidNum <= 0)
			{
				return -1;
			}
			if (!GaiaBinaryFileExists)
			{
				return -1;
			}
			if (GaiaBinaryData.Count < 100)
			{
				GaiaBinaryFileExists = ReadGaiaBinariesFile();
				if (!GaiaBinaryFileExists)
				{
					return -1;
				}
			}
			int num = 0;
			int num2 = GaiaBinaryData.Count - 1;
			do
			{
				int num3 = (num2 + num) / 2;
				if (AsteroidNum == GaiaBinaryData[num3].Asteroid)
				{
					return num3;
				}
				if (AsteroidNum < GaiaBinaryData[num3].Asteroid)
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

		internal static bool ReadGaiaBinariesFile()
		{
			if (!File.Exists(GaiaBinaryFile))
			{
				http.GetGaiaBinariesFile();
			}
			if (!File.Exists(GaiaBinaryFile))
			{
				return false;
			}
			GaiaBinaryData.Clear();
			using StreamReader streamReader = new StreamReader(GaiaBinaryFile);
			do
			{
				GaiaBinaries gaiaBinaries = new GaiaBinaries();
				gaiaBinaries.DecodeInputLine(streamReader.ReadLine());
				GaiaBinaryData.Add(gaiaBinaries);
			}
			while (!streamReader.EndOfStream);
			return true;
		}

		internal void DecodeInputLine(string X)
		{
			string[] array = X.Split(new char[1] { ',' });
			int.TryParse(array[0], out asteroid);
			double.TryParse(array[1], out period);
			double.TryParse(array[2], out sep1);
			double.TryParse(array[3], out sep1Uncert);
			double.TryParse(array[4], out diaRatio1);
			double.TryParse(array[5], out sep2);
			double.TryParse(array[6], out sep2Uncert);
			double.TryParse(array[7], out diaRatio2);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Gaia possible binary asteroid\r\n\r\nAsteroid #" + Asteroid + string.Format("\r\nPeriod (hrs) {0,1:f2}\r\n\r\nFirst possible solution\r\nSeparation ", Period));
			if (sep1 < 0.05)
			{
				stringBuilder.Append("not specified\r\n");
			}
			else
			{
				stringBuilder.AppendFormat(" {0,1:f1} ± {1,1:f1}km\r\n", sep1, sep1Uncert);
			}
			stringBuilder.AppendFormat("diameter ratio = {0,1:f2}\r\n\r\n", diaRatio1);
			stringBuilder.Append("Second possible solution\r\nSeparation ");
			if (sep2 < 0.05)
			{
				stringBuilder.Append("not specified\r\n");
			}
			else
			{
				stringBuilder.AppendFormat(" {0,1:f1} ± {1,1:f1}km\r\n", sep2, sep2Uncert);
			}
			stringBuilder.AppendFormat("diameter ratio = {0,1:f2}\r\n", diaRatio2);
			return stringBuilder.ToString();
		}

		public string GaiaBinaryDiaDurn(double MainDiameter, double MainDuration)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("#1[Sepn");
			if (sep1 < 0.05)
			{
				stringBuilder.Append(" ?km, dia ?km, Durn ?s]");
			}
			else
			{
				if (sep1 > 400.0)
				{
					stringBuilder.AppendFormat(" >{0,1:f0} km", sep1);
				}
				else if (sep1 > 5.0)
				{
					stringBuilder.AppendFormat(" {0,1:f0} ±{1,1:f0}km", sep1, sep1Uncert);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,1:f1} ± {1,1:f1}km", sep1, sep1Uncert);
				}
				stringBuilder.AppendFormat(", dia {0,1:f1}km, Durn {1,1:f2}s]", diaRatio1 * MainDiameter, diaRatio1 * MainDuration);
			}
			stringBuilder.Append(" #2[Sepn");
			if (sep2 < 0.05)
			{
				stringBuilder.Append(" ?km, dia ?km, Durn ?s]");
			}
			else
			{
				if (sep2 > 400.0)
				{
					stringBuilder.AppendFormat(" >{0,1:f0}km", sep2);
				}
				else if (sep2 > 5.0)
				{
					stringBuilder.AppendFormat(" {0,1:f0} ±{1,1:f0}km", sep2, sep2Uncert);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,1:f1} ± {1,1:f1}km", sep2, sep1Uncert);
				}
				stringBuilder.AppendFormat(", dia {0,1:f1}km, Durn {1,1:f2}s]", diaRatio2 * MainDiameter, diaRatio2 * MainDuration);
			}
			return stringBuilder.ToString();
		}
	}
}

using System;
using System.Collections.Generic;
using System.IO;

namespace Occult.Star_Catalogues
{
	internal class StarDiameters
	{
		internal static List<StarDiameters> StarDia = new List<StarDiameters>();

		private double ra;

		private double dec;

		private double mag;

		private double dia;

		private int numMeasures;

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

		internal double Mag
		{
			get
			{
				return mag;
			}
			set
			{
				mag = value;
			}
		}

		internal double Dia
		{
			get
			{
				return dia;
			}
			set
			{
				dia = value;
			}
		}

		internal int NumMeasures
		{
			get
			{
				return numMeasures;
			}
			set
			{
				numMeasures = value;
			}
		}

		internal static bool Charm2CadarsStarDia(double RAhrs, double DecDeg, double Mag, out double Dia, out int NumberMeasures)
		{
			Dia = 0.0;
			NumberMeasures = 0;
			if (StarDia.Count == 0)
			{
				FillStarDiamList();
			}
			if (StarDia.Count == 0)
			{
				return false;
			}
			int num = StarDia.Count - 1;
			int num2 = 0;
			double num3 = RAhrs - 0.0001;
			if (num3 < 0.0)
			{
				num3 = 0.0;
			}
			int num4;
			do
			{
				num4 = (num + num2) / 2;
				if (num4 < 0)
				{
					num4 = 0;
				}
				if (num3 < StarDia[num4].RA)
				{
					num = num4 - 1;
				}
				else
				{
					num2 = num4 + 1;
				}
			}
			while (num2 <= num);
			num4--;
			if (num4 < 0)
			{
				num4 = 0;
			}
			do
			{
				if ((Math.Abs(StarDia[num4].Dec - DecDeg) < 0.001) & (Math.Abs(StarDia[num4].Mag - Mag) < 1.0))
				{
					Dia = StarDia[num4].Dia;
					NumberMeasures = StarDia[num4].NumMeasures;
					return true;
				}
				if (StarDia[num4].RA - num3 > 0.0001)
				{
					return false;
				}
				num4++;
			}
			while (num4 < StarDia.Count);
			return false;
		}

		internal static void FillStarDiamList()
		{
			StarDia = new List<StarDiameters>();
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\StarDia.bin"))
			{
				return;
			}
			using FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\StarDia.bin", FileMode.Open, FileAccess.Read);
			int num = 0;
			int num2 = (int)fileStream.Length / 13;
			using BinaryReader binaryReader = new BinaryReader(fileStream);
			for (num = 0; num < num2; num++)
			{
				StarDiameters starDiameters = new StarDiameters();
				fileStream.Seek(13 * num, SeekOrigin.Begin);
				starDiameters.RA = binaryReader.ReadSingle();
				starDiameters.Dec = binaryReader.ReadSingle();
				starDiameters.Mag = (double)binaryReader.ReadInt16() / 10.0;
				starDiameters.Dia = (double)binaryReader.ReadInt16() / 10000.0;
				starDiameters.NumMeasures = binaryReader.ReadByte();
				if ((starDiameters.NumMeasures > 1) & (starDiameters.Dia > 0.0001))
				{
					StarDia.Add(starDiameters);
				}
			}
		}
	}
}

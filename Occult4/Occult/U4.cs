using System;

namespace Occult
{
	internal class U4
	{
		private const double MilliSec_InRadian = 648000000.0 / Math.PI;

		private double ra2000mas;

		private double dec2000_mas;

		private double mag = 30.0;

		private double pmRA_mas;

		private double pmDecmas;

		private double cosDecFactor;

		private uint number;

		internal double RA2000_mas
		{
			get
			{
				return ra2000mas;
			}
			set
			{
				ra2000mas = value;
			}
		}

		internal double Dec2000_mas
		{
			get
			{
				return dec2000_mas;
			}
			set
			{
				dec2000_mas = value;
				cosDecFactor = Math.Cos(dec2000_mas / (648000000.0 / Math.PI));
			}
		}

		internal double ApertureMag
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

		internal uint Number
		{
			get
			{
				return number;
			}
			set
			{
				number = value;
			}
		}

		internal double PmRA_mas
		{
			get
			{
				return pmRA_mas;
			}
			set
			{
				pmRA_mas = value;
			}
		}

		internal double PMDecmas
		{
			get
			{
				return pmDecmas;
			}
			set
			{
				pmDecmas = value;
			}
		}

		internal double RA2016mas => ra2000mas + 16.0 * pmRA_mas / cosDecFactor;

		internal double Dec2016mas => dec2000_mas + 16.0 * pmDecmas;
	}
}

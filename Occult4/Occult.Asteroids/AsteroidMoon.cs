namespace Occult.Asteroids
{
	public class AsteroidMoon
	{
		private string name = "";

		private double diameter;

		private double distKM;

		private double pA = -1.0;

		public string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		public double Diameter
		{
			get
			{
				return diameter;
			}
			set
			{
				diameter = value;
			}
		}

		public double DistKM
		{
			get
			{
				return distKM;
			}
			set
			{
				distKM = value;
			}
		}

		public double PA
		{
			get
			{
				return pA;
			}
			set
			{
				pA = value;
			}
		}
	}
}
